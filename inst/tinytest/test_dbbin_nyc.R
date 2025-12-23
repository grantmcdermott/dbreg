# NYC Taxi dbbin tests
# NOTE: these tests are only run if two conditions are met
#   1. Environment variable DBREG_TEST_NYC=TRUE
#   2. NYC taxi data at {repo}/nyc-taxi/year=2012

if (!tolower(Sys.getenv("DBREG_TEST_NYC")) %in% c("true", "1")) {
  exit_file("Run `Sys.setenv(DBREG_TEST_NYC = 'TRUE')` to enable NYC taxi tests")
}

nyc_path = here::here("nyc-taxi/year=2012")
if (!dir.exists(nyc_path)) {
  exit_file("NYC taxi data not found at nyc-taxi/year=2012")
}
library(dbreg)
library(DBI)
library(duckdb)

# Connect to DuckDB and register NYC taxi data (one month)
con = dbConnect(duckdb())
dbExecute(con, sprintf("
  CREATE VIEW nyc_jan AS
  SELECT *
  FROM read_parquet('%s/month=1/*.parquet')
", nyc_path))

#
## Test 1: Basic binning with quantile partition_method ----
#

bins_quantile = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 20, 
  degree = 1, 
  partition_method = "quantile",
  conn = con,
  verbose = FALSE
)

# Check S3 structure
expect_true(inherits(bins_quantile, "dbbin"))
expect_true(inherits(bins_quantile, "tbl_df"))
expect_equal(nrow(bins_quantile), 20)
expect_true(all(c("bin", "x_left", "x_right", "x_mid", "n", "y_left", "y_right") %in% names(bins_quantile)))

# Check metadata attributes
expect_true(!is.null(attr(bins_quantile, "formula")))
expect_true(!is.null(attr(bins_quantile, "fit")))
expect_equal(unique(bins_quantile$partition_method), "quantile")
expect_equal(unique(bins_quantile$degree), 1)


#
## Test 2: Equal-width partition_method ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
bins_equal = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 15,
  degree = 1,
  partition_method = "equal",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_equal), 15)
expect_equal(unique(bins_equal$partition_method), "equal")

# Equal-width should have (nearly) constant bin width
bin_widths = bins_equal$x_right - bins_equal$x_left
expect_true(max(bin_widths) / min(bin_widths) < 1.1)  # Within 10% variation


#
## Test 3: Binning with controls ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
bins_controls = dbbin(
  fare_amount ~ trip_distance + passenger_count,
  "nyc_jan",
  B = 10,
  degree = 1,
  partition_method = "quantile",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_controls), 10)
# Check that formula includes passenger_count
expect_true(grepl("passenger_count", deparse(attr(bins_controls, "formula"))))


#
## Test 4: Plot method works ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)

# Test that plot doesn't error (both backends)
bins_plot = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 10,
  degree = 1,
  conn = con,
  verbose = FALSE
)

# Test base graphics
pdf(NULL)  # Suppress output
expect_silent(plot(bins_plot, backend = "base"))
dev.off()

# Test auto backend
pdf(NULL)
expect_silent(plot(bins_plot, backend = "auto"))
dev.off()

# Test degree = 0 (step function)
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
bins_means = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 10,
  degree = 0,
  conn = con,
  verbose = FALSE
)

pdf(NULL)
expect_silent(plot(bins_means, backend = "auto"))
dev.off()


#
## Test 5: Constrained binscatter - level continuity (smooth = 1) ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)

bins_smooth1 = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 20,
  degree = 2,
  smooth = 1,
  partition_method = "equal",
  conn = con,
  verbose = FALSE
)

# Check that level continuity is enforced at bin boundaries
# Right endpoint of bin b should equal left endpoint of bin b+1
# Note: x_right[b] and x_left[b+1] are empirical min/max, so small gaps expected
for (b in 1:(nrow(bins_smooth1) - 1)) {
  expect_equal(
    bins_smooth1$y_right[b], 
    bins_smooth1$y_left[b + 1],
    tolerance = 0.1,  # Allow for gap between empirical bin boundaries
    info = sprintf("Level discontinuity at boundary %d", b)
  )
}

# Compare to unconstrained: should be different
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
bins_smooth0 = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 20,
  degree = 2,
  smooth = 0,
  partition_method = "equal",
  conn = con,
  verbose = FALSE
)

# Constrained and unconstrained should give different results
expect_true(
  mean(abs(bins_smooth1$y_left - bins_smooth0$y_left)) > 0.01,
  info = "Constrained and unconstrained fits should differ"
)

## Test 6: Constrained binscatter - C1 continuity (smooth = 2) ----
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)

bins_smooth2 = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  B = 8,
  degree = 2,
  smooth = 2,
  partition_method = "quantile",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_smooth2), 8)
expect_equal(unique(bins_smooth2$smooth), 2)
expect_equal(unique(bins_smooth2$degree), 2)

# Again, test for continuity at boundaries
# Note: x_right[b] and x_left[b+1] are empirical min/max, so small gaps expected
for (b in 1:(nrow(bins_smooth2) - 1)) {
  expect_equal(
    bins_smooth2$y_right[b], 
    bins_smooth2$y_left[b + 1],
    tolerance = 0.1,  # Allow for gap between empirical bin boundaries
    info = sprintf("Level discontinuity at boundary %d (smooth=2)", b)
  )
}

# Plot should work with constrained estimation
pdf(NULL)
expect_silent(plot(bins_smooth2, backend = "auto"))
dev.off()


#
## Test 7: Constrained binscatter with controls and FE (smooth = 2) ----
#
# This tests the spline-based approach with full complexity:
# - Controls (passenger_count) absorbed linearly
# - Fixed effects (payment_type) absorbed via demeaning
# - C1 continuity enforced via truncated-power basis
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)

bins_full = dbbin(
  fare_amount ~ trip_distance + passenger_count | vendor_name,
  "nyc_jan",
  B = 10,
  degree = 2,
  smooth = 2,
  partition_method = "equal",
  ci = TRUE,
  conn = con,
  verbose = TRUE
)

# Check structure
expect_equal(nrow(bins_full), 10)
expect_equal(unique(bins_full$smooth), 2)
expect_equal(unique(bins_full$degree), 2)

# Check that CI columns exist (since ci=TRUE)
expect_true("se_left" %in% names(bins_full))
expect_true("se_mid" %in% names(bins_full))
expect_true("se_right" %in% names(bins_full))
expect_true("ci_low_left" %in% names(bins_full))
expect_true("ci_high_right" %in% names(bins_full))

# Verify approximate level continuity at boundaries
# Note: x_right[b] and x_left[b+1] are empirical min/max from data,
# so there's a small gap between them. The spline is continuous at any x,
# but we're evaluating at slightly different x values.
for (b in 1:(nrow(bins_full) - 1)) {
  expect_equal(
    bins_full$y_right[b], 
    bins_full$y_left[b + 1],
    tolerance = 0.1,  # Allow for gap between empirical bin boundaries
    info = sprintf("Level discontinuity at boundary %d (smooth=2 with controls+FE)", b)
  )
}

# Check that fit object has the spline basis coefficients
fit = attr(bins_full, "fit")
expect_true(!is.null(fit))
coef_names = rownames(fit$coeftable)
# Should have x_spline, x2_spline, knot*_pow2 terms
expect_true(any(grepl("x_spline", coef_names)))
expect_true(any(grepl("x2_spline", coef_names)))
expect_true(any(grepl("knot.*_pow2", coef_names)))
# Should have passenger_count control
expect_true("passenger_count" %in% coef_names)


# Cleanup
dbDisconnect(con, shutdown = TRUE)
