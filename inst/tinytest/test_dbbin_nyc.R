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


# Cleanup
dbDisconnect(con, shutdown = TRUE)
