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
devtools::load_all()
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
## Test 1: Basic binning with quantile partition ----
#

bins_quantile = dbbin(
  "nyc_jan", 
  y = fare_amount,
  x = trip_distance,
  B = 20, 
  degree = 1, 
  partition = "quantile",
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
expect_equal(unique(bins_quantile$partition), "quantile")
expect_equal(unique(bins_quantile$degree), 1)


#
## Test 2: Equal-width partition ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
bins_equal = dbbin(
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
  B = 15,
  degree = 1,
  partition = "equal",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_equal), 15)
expect_equal(unique(bins_equal$partition), "equal")

# Equal-width should have (nearly) constant bin width
bin_widths = bins_equal$x_right - bins_equal$x_left
expect_true(max(bin_widths) / min(bin_widths) < 1.1)  # Within 10% variation


#
## Test 3: Binning with controls ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
bins_controls = dbbin(
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
  B = 10,
  degree = 1,
  controls = ~ passenger_count,
  partition = "quantile",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_controls), 10)
expect_true(grepl("controls", deparse(attr(bins_controls, "formula"))))


#
## Test 4: Plot method works ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)

# Test that plot doesn't error (both backends)
bins_plot = dbbin(
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
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
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
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
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
  B = 20,
  degree = 2,
  smooth = 1,
  partition = "equal",
  conn = con,
  verbose = FALSE
)
plot(bins_smooth1)

# Check continuity manually
for (b in 1:19) {
  cat(sprintf("Boundary %d: y_right[%d]=%.4f, y_left[%d]=%.4f, diff=%.6f\n",
              b, b, bins_smooth1$y_right[b], b+1, bins_smooth1$y_left[b+1],
              bins_smooth1$y_right[b] - bins_smooth1$y_left[b+1]))
}
expect_equal(nrow(bins_smooth1), 10)
expect_equal(unique(bins_smooth1$smooth), 1)
expect_equal(unique(bins_smooth1$degree), 1)

# Check that level continuity is enforced at bin boundaries
# Right endpoint of bin b should equal left endpoint of bin b+1
for (b in 1:(nrow(bins_smooth1) - 1)) {
  expect_equal(
    bins_smooth1$y_right[b], 
    bins_smooth1$y_left[b + 1],
    tolerance = 1e-6,
    info = sprintf("Level discontinuity at boundary %d", b)
  )
}

# Compare to unconstrained: should be different
bins_smooth0 = dbbin(
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
  B = 10,
  degree = 1,
  smooth = 0,
  partition = "quantile",
  conn = con,
  verbose = FALSE
)

# Constrained and unconstrained should give different results
# (unless data happens to be perfectly continuous, which is unlikely)
expect_true(
  mean(abs(bins_smooth1$y_left - bins_smooth0$y_left)) > 0.01,
  info = "Constrained and unconstrained fits should differ"
)


#
## Test 6: Constrained binscatter - C1 continuity (smooth = 2) ----
#
try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)

bins_smooth2 = dbbin(
  "nyc_jan",
  y = fare_amount,
  x = trip_distance,
  B = 8,
  degree = 2,
  smooth = 2,
  partition = "quantile",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_smooth2), 8)
expect_equal(unique(bins_smooth2$smooth), 2)
expect_equal(unique(bins_smooth2$degree), 2)

# With smooth=2 and degree=2, we get C1 continuity
# This is harder to test directly without computing derivatives,
# but we can verify level continuity holds
for (b in 1:(nrow(bins_smooth2) - 1)) {
  expect_equal(
    bins_smooth2$y_right[b], 
    bins_smooth2$y_left[b + 1],
    tolerance = 1e-6,
    info = sprintf("Level discontinuity at boundary %d (smooth=2)", b)
  )
}

# Plot should work with constrained estimation
pdf(NULL)
expect_silent(plot(bins_smooth2, backend = "auto"))
dev.off()


# Cleanup
dbDisconnect(con, shutdown = TRUE)
