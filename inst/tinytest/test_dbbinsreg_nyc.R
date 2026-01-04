# NYC Taxi dbbinsreg tests (binsreg-compatible API)
# NOTE: these tests are only run if two conditions are met
#   1. Environment variable DBREG_TEST_NYC=TRUE
#   2. NYC taxi data at {repo}/nyc-taxi/year=2012

if (!tolower(Sys.getenv("DBREG_TEST_NYC")) %in% c("true", "1")) {
  exit_file("Run Sys.setenv(DBREG_TEST_NYC = TRUE) to enable NYC taxi tests")
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

# Helper to clean up temp views between tests
cleanup = function() {
  try(dbExecute(con, "DROP VIEW IF EXISTS tmp_table_dbreg"), silent = TRUE)
}

## Test 1: Basic binning with quantile-spaced bins ----

bins_qs = dbbinsreg(
  fare_amount ~ trip_distance,
  conn = con,
  table = "nyc_jan",
  nbins = 20,
  points = c(0, 0),
  binspos = "qs",
  plot = FALSE,
  verbose = FALSE
)

expect_true(inherits(bins_qs, "dbbinsreg"))
expect_true(is.list(bins_qs))
expect_true("points" %in% names(bins_qs))
expect_true("bins" %in% names(bins_qs))
expect_equal(nrow(bins_qs$points), 20)
expect_true(all(c("x", "bin", "fit") %in% names(bins_qs$points)))
expect_equal(nrow(bins_qs$bins), 20)
expect_equal(bins_qs$opt$nbins, 20)
expect_equal(bins_qs$opt$binspos, "qs")

## Test 2: Even-spaced bins ----
cleanup()

bins_es = dbbinsreg(
  fare_amount ~ trip_distance,
  conn = con,
  table = "nyc_jan",
  nbins = 15,
  points = c(0, 0),
  binspos = "es",
  plot = FALSE,
  verbose = FALSE
)

expect_equal(nrow(bins_es$points), 15)
expect_equal(bins_es$opt$binspos, "es")
bin_widths = bins_es$bins$right - bins_es$bins$left
expect_true(max(bin_widths) / min(bin_widths) < 1.1)

## Test 3: Binning with controls ----
cleanup()

bins_ctrl = dbbinsreg(
  fare_amount ~ trip_distance + passenger_count,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(0, 0),
  plot = FALSE,
  verbose = FALSE
)

expect_equal(nrow(bins_ctrl$points), 10)
expect_true(grepl("passenger_count", deparse(bins_ctrl$opt$formula)))

## Test 4: Piecewise linear with continuity ----
cleanup()

bins_linear = dbbinsreg(
  fare_amount ~ trip_distance,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(1, 1),
  plot = FALSE,
  verbose = FALSE
)

expect_equal(nrow(bins_linear$points), 10)
expect_equal(bins_linear$opt$points[1], 1)
expect_equal(bins_linear$opt$points[2], 1)

## Test 5: Points and line with different specs ----
cleanup()

bins_mixed = dbbinsreg(
  fare_amount ~ trip_distance,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(0, 0),
  line = c(1, 1),
  plot = FALSE,
  verbose = FALSE
)

expect_true("points" %in% names(bins_mixed))
expect_true("line" %in% names(bins_mixed))
expect_equal(nrow(bins_mixed$points), 10)
expect_true(nrow(bins_mixed$line) > 10)

## Test 6: Confidence intervals with HC1 ----
cleanup()

bins_ci = dbbinsreg(
  fare_amount ~ trip_distance,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(0, 0),
  ci = TRUE,
  plot = FALSE,
  verbose = FALSE
)

expect_true("se" %in% names(bins_ci$points))
expect_true("lwr" %in% names(bins_ci$points))
expect_true("upr" %in% names(bins_ci$points))
se_range = diff(range(bins_ci$points$se))
expect_true(se_range > 0, info = "HC1 SEs should vary by bin")
expect_true(all(bins_ci$points$lwr < bins_ci$points$fit))
expect_true(all(bins_ci$points$upr > bins_ci$points$fit))

## Test 7: With fixed effects ----
cleanup()

bins_fe = dbbinsreg(
  fare_amount ~ trip_distance | vendor_name,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(0, 0),
  plot = FALSE,
  verbose = FALSE
)

expect_equal(nrow(bins_fe$points), 10)

## Test 8: Piecewise quadratic with C1 continuity ----
cleanup()

bins_quad = dbbinsreg(
  fare_amount ~ trip_distance,
  conn = con,
  table = "nyc_jan",
  nbins = 8,
  points = c(2, 1),
  binspos = "qs",
  plot = FALSE,
  verbose = FALSE
)

expect_equal(nrow(bins_quad$points), 8)
expect_equal(bins_quad$opt$points[1], 2)
expect_equal(bins_quad$opt$points[2], 1)

# Cleanup
dbDisconnect(con, shutdown = TRUE)
