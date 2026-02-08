library(dbreg)

# -----------------------------------------------------------------------------
# Exactness checks for dbkreg (small data, dense grid)
# -----------------------------------------------------------------------------

kernel_epan = function(u) {
  w = 0.75 * (1 - u^2)
  w[abs(u) > 1] = 0
  w
}

kreg_lc = function(x0, x, y, h) {
  u = (x - x0) / h
  w = kernel_epan(u)
  sum(w * y) / sum(w)
}

kreg_ll = function(x0, x, y, h) {
  u = x - x0
  w = kernel_epan(u / h)
  s0 = sum(w)
  s1 = sum(w * u)
  s2 = sum(w * u * u)
  t0 = sum(w * y)
  t1 = sum(w * u * y)
  (s2 * t0 - s1 * t1) / (s0 * s2 - s1 * s1)
}

set.seed(42)
n = 20
x = seq(-1, 1, length.out = n)

grid = seq(min(x), max(x), length.out = 2000)
bandwidth = 0.6

# Local constant: compare to R reference
dat = data.frame(x = x, y = sin(2 * x))
fit_lc = dbkreg(
  y ~ x,
  data = dat,
  eval = "points",
  grid = grid,
  bandwidth = bandwidth,
  kernel = "epanechnikov",
  degree = 0
)

ref_lc = vapply(grid, kreg_lc, numeric(1), x = dat$x, y = dat$y, h = bandwidth)

expect_equal(
  fit_lc$grid$fit,
  ref_lc,
  tolerance = 1e-10,
  info = "dbkreg (local constant) matches R reference on dense grid"
)

# Local linear: exactness for linear function
dat_lin = data.frame(x = x, y = 1 + 2 * x)
fit_ll = dbkreg(
  y ~ x,
  data = dat_lin,
  eval = "points",
  grid = grid,
  bandwidth = bandwidth,
  kernel = "epanechnikov",
  degree = 1
)

expect_equal(
  fit_ll$grid$fit,
  1 + 2 * grid,
  tolerance = 1e-10,
  info = "dbkreg (local linear) recovers linear function exactly on dense grid"
)

# Confidence interval path for degree 0 and 1
fit_ci_lc = dbkreg(
  y ~ x,
  data = dat,
  eval = "points",
  grid = grid,
  bandwidth = 0.8,
  kernel = "epanechnikov",
  degree = 0,
  ci = TRUE
)

fit_ci_ll = dbkreg(
  y ~ x,
  data = dat,
  eval = "points",
  grid = grid,
  bandwidth = 0.8,
  kernel = "epanechnikov",
  degree = 1,
  ci = TRUE
)

for (obj in list(fit_ci_lc, fit_ci_ll)) {
  expect_true(all(c("se", "lwr", "upr") %in% names(obj$grid)))
  finite_idx = is.finite(obj$grid$fit) & is.finite(obj$grid$lwr) & is.finite(obj$grid$upr)
  expect_true(all(obj$grid$lwr[finite_idx] <= obj$grid$fit[finite_idx]))
  expect_true(all(obj$grid$fit[finite_idx] <= obj$grid$upr[finite_idx]))
}

# eval = "data" should return unique x values in sorted order
dat_dup = data.frame(
  x = rep(seq(-1, 1, length.out = 10), each = 2),
  y = rep(seq(-1, 1, length.out = 10), each = 2)
)
fit_data = dbkreg(
  y ~ x,
  data = dat_dup,
  eval = "data",
  bandwidth = 0.6,
  kernel = "epanechnikov",
  degree = 0
)
expect_equal(
  nrow(fit_data$grid),
  length(unique(dat_dup$x)),
  info = "eval='data' returns one grid point per unique x value"
)

# 2D kernel regression path
set.seed(7)
n2 = 300
x1 = runif(n2)
x2 = runif(n2)
y2 = sin(2 * x1) + cos(2 * x2)
dat2 = data.frame(y = y2, x1 = x1, x2 = x2)
grid2 = expand.grid(x1 = c(0.2, 0.5, 0.8), x2 = c(0.1, 0.6, 0.9))

fit_2d = dbkreg(
  y ~ x1 + x2,
  data = dat2,
  eval = "points",
  grid = grid2,
  bandwidth = c(0.25, 0.25),
  kernel = "epanechnikov",
  degree = 1
)

expect_equal(
  nrow(fit_2d$grid),
  nrow(grid2),
  info = "2D dbkreg returns one fit per grid row"
)
expect_true(
  all(is.finite(fit_2d$grid$fit)),
  info = "2D dbkreg returns finite fitted values"
)

# randcut smoke test
fit_randcut = dbkreg(
  y ~ x,
  data = dat,
  eval = "grid",
  n_eval = 25,
  bandwidth = 0.6,
  randcut = 0.5,
  kernel = "epanechnikov",
  degree = 0
)
expect_true(nrow(fit_randcut$grid) >= 1 && nrow(fit_randcut$grid) <= 25, info = "randcut returns at most n_eval grid points")
expect_true(all(is.finite(fit_randcut$grid$fit)))

# Kernel variant smoke tests
for (k in c("uniform", "biweight", "triweight")) {
  fit_k = dbkreg(
    y ~ x,
    data = dat,
    eval = "grid",
    n_eval = 15,
    bandwidth = 0.7,
    kernel = k,
    degree = 0
  )
  expect_true(all(is.finite(fit_k$grid$fit)), info = paste("kernel", k, "returns finite fits"))
}

# Print and plot methods
print_txt = capture.output(print(fit_ll))
expect_true(any(grepl("kernel regression", print_txt)))
expect_true(any(grepl("Bandwidth", print_txt)))

plot_ret = plot(fit_ll)
expect_true(inherits(plot_ret, "dbkreg"), info = "plot.dbkreg returns dbkreg object invisibly")

tinyplot_ret = tinyplot::tinyplot(fit_ll)
expect_true(inherits(tinyplot_ret, "dbkreg"), info = "tinyplot.dbkreg returns dbkreg object invisibly")

expect_error(
  plot(fit_2d),
  "1D",
  info = "plot.dbkreg errors clearly for multi-dimensional fits"
)
expect_error(
  tinyplot::tinyplot(fit_2d),
  "1D",
  info = "tinyplot.dbkreg errors clearly for multi-dimensional fits"
)

# -----------------------------------------------------------------------------
# NYC Taxi tests (large data smoke tests)
# -----------------------------------------------------------------------------

if (!tolower(Sys.getenv("DBREG_TEST_NYC")) %in% c("true", "1")) {
  exit_file("Run `Sys.setenv(DBREG_TEST_NYC = 'TRUE')` to enable NYC taxi tests")
}

nyc_path = here::here("nyc-taxi/year=2012")
if (!dir.exists(nyc_path)) {
  exit_file("NYC taxi data not found at nyc-taxi/year=2012")
}

# Create a DuckDB connection with one month of NYC data
con = DBI::dbConnect(duckdb::duckdb(), shutdown = TRUE)

DBI::dbExecute(con, sprintf("
  CREATE VIEW nyc_kreg AS
  SELECT
    tip_amount,
    fare_amount,
    trip_distance
  FROM read_parquet('%s/month=1/*.parquet')
  WHERE trip_distance > 0 AND trip_distance < 20
    AND fare_amount > 0 AND fare_amount < 100
", nyc_path))

# Test local constant kernel regression on NYC data
nyc_lc = dbkreg(
  tip_amount ~ fare_amount,
  conn = con,
  table = "nyc_kreg",
  eval = "grid",
  n_eval = 20,
  grid_method = "quantile",
  bandwidth = 5,
  kernel = "epanechnikov",
  degree = 0
)

expect_true(
  inherits(nyc_lc, "dbkreg"),
  info = "dbkreg local constant returns dbkreg object on NYC data"
)
expect_equal(
  nrow(nyc_lc$grid), 20,
  info = "dbkreg returns correct number of evaluation points"
)
expect_true(
  all(is.finite(nyc_lc$grid$fit)),
  info = "dbkreg local constant fits are all finite on NYC data"
)

# Test local linear kernel regression on NYC data
nyc_ll = dbkreg(
  tip_amount ~ fare_amount,
  conn = con,
  table = "nyc_kreg",
  eval = "grid",
  n_eval = 20,
  grid_method = "quantile",
  bandwidth = 5,
  kernel = "epanechnikov",
  degree = 1
)

expect_true(
  inherits(nyc_ll, "dbkreg"),
  info = "dbkreg local linear returns dbkreg object on NYC data"
)
expect_true(
  all(is.finite(nyc_ll$grid$fit)),
  info = "dbkreg local linear fits are all finite on NYC data"
)

# Check that tip increases with fare (sanity check)
expect_true(
  cor(nyc_ll$grid$fare_amount, nyc_ll$grid$fit) > 0.5,
  info = "dbkreg shows positive relationship between fare and tip"
)

DBI::dbDisconnect(con)
rm(con)
