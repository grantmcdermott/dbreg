# NYC Taxi dbbin tests (binsreg-compatible API)
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

bins_qs = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  nbins = 20,
  dots = c(0, 0),
  binspos = "qs",
  conn = con,
  verbose = FALSE
)

expect_true(inherits(bins_qs, "dbbin"))
expect_true(is.list(bins_qs))
expect_true("data.dots" %in% names(bins_qs))
expect_true("data.bin" %in% names(bins_qs))
expect_equal(nrow(bins_qs$data.dots), 20)
expect_true(all(c("x", "bin", "fit") %in% names(bins_qs$data.dots)))
expect_equal(nrow(bins_qs$data.bin), 20)
expect_equal(bins_qs$opt$nbins, 20)
expect_equal(bins_qs$opt$binspos, "qs")

## Test 2: Even-spaced bins ----
cleanup()

bins_es = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  nbins = 15,
  dots = c(0, 0),
  binspos = "es",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_es$data.dots), 15)
expect_equal(bins_es$opt$binspos, "es")
bin_widths = bins_es$data.bin$right.endpoint - bins_es$data.bin$left.endpoint
expect_true(max(bin_widths) / min(bin_widths) < 1.1)

## Test 3: Binning with controls ----
cleanup()

bins_ctrl = dbbin(
  fare_amount ~ trip_distance + passenger_count,
  "nyc_jan",
  nbins = 10,
  dots = c(0, 0),
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_ctrl$data.dots), 10)
expect_true(grepl("passenger_count", deparse(bins_ctrl$opt$formula)))

## Test 4: Piecewise linear with continuity ----
cleanup()

bins_linear = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  nbins = 10,
  dots = c(1, 1),
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_linear$data.dots), 10)
expect_equal(bins_linear$opt$dots[1], 1)
expect_equal(bins_linear$opt$dots[2], 1)

## Test 5: Dots and line with different specs ----
cleanup()

bins_mixed = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  nbins = 10,
  dots = c(0, 0),
  line = c(1, 1),
  conn = con,
  verbose = FALSE
)

expect_true("data.dots" %in% names(bins_mixed))
expect_true("data.line" %in% names(bins_mixed))
expect_equal(nrow(bins_mixed$data.dots), 10)
expect_true(nrow(bins_mixed$data.line) > 10)

## Test 6: Confidence intervals with HC1 ----
cleanup()

bins_ci = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  nbins = 10,
  dots = c(0, 0),
  ci = TRUE,
  conn = con,
  verbose = FALSE
)

expect_true("se" %in% names(bins_ci$data.dots))
expect_true("ci.l" %in% names(bins_ci$data.dots))
expect_true("ci.r" %in% names(bins_ci$data.dots))
se_range = diff(range(bins_ci$data.dots$se))
expect_true(se_range > 0, info = "HC1 SEs should vary by bin")
expect_true(all(bins_ci$data.dots$ci.l < bins_ci$data.dots$fit))
expect_true(all(bins_ci$data.dots$ci.r > bins_ci$data.dots$fit))

## Test 7: With fixed effects ----
cleanup()

bins_fe = dbbin(
  fare_amount ~ trip_distance | vendor_name,
  "nyc_jan",
  nbins = 10,
  dots = c(0, 0),
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_fe$data.dots), 10)

## Test 8: Piecewise quadratic with C1 continuity ----
cleanup()

bins_quad = dbbin(
  fare_amount ~ trip_distance,
  "nyc_jan",
  nbins = 8,
  dots = c(2, 1),
  binspos = "qs",
  conn = con,
  verbose = FALSE
)

expect_equal(nrow(bins_quad$data.dots), 8)
expect_equal(bins_quad$opt$dots[1], 2)
expect_equal(bins_quad$opt$dots[2], 1)

# Cleanup
dbDisconnect(con, shutdown = TRUE)
