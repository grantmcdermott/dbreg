# NYC Taxi dbbinsreg tests
# These are smoke tests for large data - correctness is tested in test_dbbinsreg_binsreg.R
#
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

#
## Test 1: Full complexity (controls + FE) ----

bins_full = dbbinsreg(
  fare_amount ~ trip_distance + passenger_count | vendor_name,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(0, 0),
  ci = TRUE,
  plot = FALSE,
  verbose = FALSE
)

expect_true(inherits(bins_full, "dbbinsreg"))
expect_equal(nrow(bins_full$points), 10)
expect_true(all(c("x", "bin", "fit", "se", "lwr", "upr") %in% names(bins_full$points)))
expect_true(grepl("passenger_count", deparse(bins_full$opt$formula)))

#
## Test 2: Spline regression with sampling (s > 0) ----

bins_spline = dbbinsreg(
  fare_amount ~ trip_distance | vendor_name,
  conn = con,
  table = "nyc_jan",
  nbins = 10,
  points = c(1, 1),
  line = c(1, 1),
  sample_fit = TRUE,
  plot = FALSE,
  verbose = FALSE
)

expect_equal(nrow(bins_spline$points), 10)
expect_true("line" %in% names(bins_spline))
expect_true(nrow(bins_spline$line) > 10)
expect_equal(bins_spline$opt$points, c(1, 1))

# Cleanup
dbDisconnect(con, shutdown = TRUE)

