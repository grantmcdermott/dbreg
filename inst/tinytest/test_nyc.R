# NYC Taxi data tests
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

# Path to hive-partitioned parquet files
nyc_parquet = sprintf("read_parquet('%s/../**/*.parquet')", nyc_path)

#
## Test compress strategy with HC1 ----

nyc_compress = dbreg(
  tip_amount ~ fare_amount + passenger_count | month + vendor_name,
  path = nyc_parquet,
  vcov = "hc1",
  strategy = "compress",
  verbose = FALSE
)

# benchmarking against known results from fixest (run on a large ec2 instance)
expect_equal(nyc_compress$coeftable["fare_amount", "estimate"], 0.106744, tolerance = 1e-5)
expect_equal(nyc_compress$coeftable["passenger_count", "estimate"], -0.029086, tolerance = 1e-5)
expect_equal(nyc_compress$coeftable["fare_amount", "std.error"], 6.821837e-05, tolerance = 1e-5)
expect_equal(nyc_compress$coeftable["passenger_count", "std.error"], 1.062061e-04, tolerance = 1e-5)

#
## Test clustered SEs ----

nyc_compress_cl = dbreg(
  tip_amount ~ fare_amount + passenger_count | month + vendor_name,
  path = nyc_parquet,
  vcov = ~month,
  strategy = "compress",
  verbose = FALSE
)

# Known values from fixest::feols with clustered SEs
# feols(..., vcov = ~month)
expect_equal(nyc_compress_cl$coeftable["fare_amount", "estimate"], 0.106744, tolerance = 1e-5)
expect_equal(nyc_compress_cl$coeftable["passenger_count", "estimate"], -0.029086, tolerance = 1e-5)
expect_equal(nyc_compress_cl$coeftable["fare_amount", "std.error"], 0.0006569137, tolerance = 1e-5)
expect_equal(nyc_compress_cl$coeftable["passenger_count", "std.error"], 0.0010304119, tolerance = 1e-5)

#
## Test conn + table method with subset and computed column ----

# Create a DuckDB connection with a view containing a subset of data
# and a computed column (day of week)
con = DBI::dbConnect(duckdb::duckdb(), shutdown = TRUE)

DBI::dbExecute(con, sprintf("
  CREATE VIEW nyc_subset AS
  SELECT
    tip_amount, trip_distance, passenger_count,
    vendor_name, month,
    dayofweek(dropoff_datetime) AS dofw
  FROM read_parquet('%s/../**/*.parquet')
  WHERE year = 2012 AND CAST(month AS INTEGER) <= 3
", nyc_path))

nyc_subset = dbreg(
  tip_amount ~ trip_distance + passenger_count | month + dofw + vendor_name,
  conn = con,
  table = "nyc_subset",
  vcov = ~dofw,
  strategy = "compress",
  verbose = FALSE
)

DBI::dbDisconnect(con)
rm(con)

# Known values from fixest::feols on same subset
# feols(tip_amount ~ trip_distance + passenger_count | month + dofw + vendor_name,
#       data = nyc_subset, vcov = ~dofw)
expect_equal(nyc_subset$coeftable["trip_distance", "estimate"], 0.236739, tolerance = 1e-5)
expect_equal(nyc_subset$coeftable["passenger_count", "estimate"], -0.025774, tolerance = 1e-5)
expect_equal(nyc_subset$coeftable["trip_distance", "std.error"], 0.0069853618, tolerance = 1e-5)
expect_equal(nyc_subset$coeftable["passenger_count", "std.error"], 0.0007528812, tolerance = 1e-5)

