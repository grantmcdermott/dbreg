# NYC Taxi tests (large data smoke + backend validation)
# -----------------------------------------------------------------------------

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
library(fixest)

rename_fixest_iv = function(x) {
  names(x) = sub("^fit_", "", names(x))
  x
}

con = dbConnect(duckdb::duckdb(), shutdown = TRUE)

dbExecute(con, sprintf("
  CREATE VIEW nyc_iv AS
  SELECT
    trip_distance AS x,
    fare_amount / 10.0 AS z,
    passenger_count AS fe1,
    dayofweek(dropoff_datetime) AS fe2,
    5.0 * (fare_amount / 10.0) + 0.5 * trip_distance +
      0.8 * passenger_count + 0.2 * dayofweek(dropoff_datetime) + tip_amount / 10.0 AS d,
    1.0 + 2.0 * (
      5.0 * (fare_amount / 10.0) + 0.5 * trip_distance +
        0.8 * passenger_count + 0.2 * dayofweek(dropoff_datetime) + tip_amount / 10.0
    ) +
      0.3 * trip_distance + 0.8 * passenger_count +
      0.2 * dayofweek(dropoff_datetime) + tip_amount / 20.0 AS y,
    1.0 + passenger_count / 10.0 AS w
  FROM read_parquet('%s/month=1/*.parquet')
  WHERE trip_distance > 0 AND trip_distance < 20
    AND fare_amount > 0 AND fare_amount < 100
    AND passenger_count > 0
  LIMIT 30000
", nyc_path))

nyc_local = dbGetQuery(con, "SELECT * FROM nyc_iv")

nyc_iv = dbivreg(
  y ~ x | d ~ z,
  conn = con,
  table = "nyc_iv",
  weights = "w",
  vcov = "hc1"
)

expect_true(
  inherits(nyc_iv, "dbivreg"),
  info = "dbivreg returns dbivreg object on NYC backend data"
)
expect_true(
  all(is.finite(nyc_iv$coeftable[, c("estimate", "std.error")])),
  info = "dbivreg returns finite coefficients and standard errors on NYC backend data"
)

fx_iv = feols(y ~ x | d ~ z, data = nyc_local, weights = ~w, vcov = "hc1")
fx_coef = rename_fixest_iv(coef(fx_iv))
expect_true(
  max(abs(nyc_iv$coeftable[names(fx_coef), "estimate"] - fx_coef)) < 1e-6,
  info = "dbivreg coefficients match fixest on NYC backend view"
)

fx_se = rename_fixest_iv(se(fx_iv))
expect_true(
  max(abs(nyc_iv$coeftable[names(fx_se), "std.error"] - fx_se)) < 1e-6,
  info = "dbivreg HC1 standard errors match fixest on NYC backend view"
)

nyc_iv_fe = dbivreg(
  y ~ x | fe1 + fe2 | d ~ z,
  conn = con,
  table = "nyc_iv",
  weights = "w",
  vcov = "hc1",
  strategy = "auto"
)

expect_true(
  nyc_iv_fe$strategy == "demean",
  info = "dbivreg auto strategy selects demean on NYC FE-IV backend data"
)
expect_true(
  all(is.finite(nyc_iv_fe$coeftable[, c("estimate", "std.error")])),
  info = "dbivreg FE-IV returns finite coefficients and standard errors on NYC backend data"
)

fx_iv_fe = feols(y ~ x | fe1 + fe2 | d ~ z, data = nyc_local, weights = ~w, vcov = "hc1")
fx_fe_coef = rename_fixest_iv(coef(fx_iv_fe))
expect_true(
  max(abs(nyc_iv_fe$coeftable[names(fx_fe_coef), "estimate"] - fx_fe_coef)) < 1e-6,
  info = "dbivreg FE-IV coefficients match fixest on NYC backend view"
)

fx_fe_se = rename_fixest_iv(se(fx_iv_fe))
expect_true(
  max(abs(nyc_iv_fe$coeftable[names(fx_fe_se), "std.error"] - fx_fe_se)) < 1e-6,
  info = "dbivreg FE-IV HC1 standard errors match fixest on NYC backend view"
)

dbDisconnect(con)
rm(con)
