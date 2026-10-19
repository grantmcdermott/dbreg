library(dbreg)

# Window implementation of alternating projections (AP). It is the Spark
# default; `dbreg.ap_method = "window"` runs the same code on DuckDB.

# Method selection and SQL shape ----
old = options(dbreg.ap_method = NULL)
expect_equal(dbreg:::ap_method("spark"), "window")
expect_equal(dbreg:::ap_method("duckdb"), "join")
options(dbreg.ap_method = "join")
expect_equal(dbreg:::ap_method("spark"), "join", info = "Option overrides the Spark default")
options(dbreg.ap_method = "windows")
expect_error(dbreg:::ap_method("duckdb"), 'must be "window" or "join"')
options(old)

sweeps = dbreg:::sql_ap_window_sweeps("t0", c("a", "b", "__w"), c("y_tilde", "x_tilde"), c("a", "b"), 3L)
expect_equal(lengths(regmatches(sweeps, gregexpr("FROM (", sweeps, fixed = TRUE))), 6L,
             info = "One subquery per FE per cycle")
expect_equal(lengths(regmatches(sweeps, gregexpr("OVER (PARTITION BY a)", sweeps, fixed = TRUE))), 3L * 2L * 2L,
             info = "Weighted mean (2 windows) per variable per cycle")
expect_true(startsWith(sweeps, "SELECT a, b, __w, y_tilde - SUM(__w * y_tilde) OVER (PARTITION BY b)"),
            info = "Outermost sweep is the last FE")

# Fixture ----
set.seed(147)
n = 2500L
dat = data.frame(
  fe1 = sample.int(40, n, TRUE), fe2 = sample.int(25, n, TRUE), fe3 = sample.int(10, n, TRUE),
  x1 = rnorm(n), x2 = rnorm(n), w = runif(n, 0.3, 2)
)
dat$fe1[1] = 41L # singleton group, retained as with fixef.rm = "none"
dat[["x spaced"]] = rnorm(n)
dat$category = sample(letters[1:3], n, TRUE)
dat$z = dat$x1 + rnorm(n)
dat$y = 0.7 * dat$x1 - 0.3 * dat$x2 + sin(dat$fe1) + cos(dat$fe2) + dat$fe3 / 10 + rnorm(n)
con = DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "fixture", dat)
DBI::dbExecute(con, paste(
  "CREATE TABLE decimal_fixture AS SELECT fe1, fe2,",
  "CAST(y AS DECIMAL(30,12)) AS y, CAST(x1 AS DECIMAL(30,12)) AS x1 FROM fixture"
))
dat_decimal = DBI::dbGetQuery(con, "SELECT * FROM decimal_fixture")
old = options(
  dbreg.ap_method = NULL, dbreg.ap_batch_size = NULL,
  dbreg.ap_tol = 1e-11, dbreg.ap_max_iter = 1000L
)
ssc_full = fixest::ssc(K.fixef = "full")

fit_with = function(method, batch, f, table, vcov, weights = NULL, ivreg = FALSE) {
  options(dbreg.ap_method = method, dbreg.ap_batch_size = batch)
  estimator = if (ivreg) dbreg::dbivreg else dbreg::dbreg
  estimator(f, conn = con, table = table, strategy = "demean", vcov = vcov, weights = weights)
}

# Window == join == fixest across model shapes ----
cases = list(
  list(name = "2 FE, clustered", f = y ~ x1 + x2 | fe1 + fe2, vcov = ~fe1),
  list(name = "3 FE", f = y ~ x1 + x2 | fe1 + fe2 + fe3, vcov = ~fe1),
  list(name = "names/factors", f = y ~ `x spaced` + x2 * category | fe1 + fe2, vcov = ~fe1),
  list(name = "numeric interaction, HC1", f = y ~ x1 * x2 | fe1 + fe2, vcov = "hc1"),
  list(name = "cluster var in RHS", f = y ~ x1 + fe3 | fe1 + fe2, vcov = ~fe3),
  list(name = "weighted", f = y ~ x1 + x2 | fe1 + fe2, vcov = ~fe1, weights = "w"),
  list(name = "decimal inputs", f = y ~ x1 | fe1 + fe2, vcov = ~fe1, table = "decimal_fixture", data = dat_decimal),
  list(name = "IV", f = y ~ x2 | fe1 + fe2 | x1 ~ z, vcov = ~fe1, ivreg = TRUE)
)
for (case in cases) {
  table = if (is.null(case$table)) "fixture" else case$table
  data = if (is.null(case$data)) dat else case$data
  ivreg = isTRUE(case$ivreg)
  join = fit_with("join", 1L, case$f, table, case$vcov, case$weights, ivreg)
  ref_args = list(
    case$f, data = data, vcov = if (identical(case$vcov, "hc1")) "hetero" else case$vcov,
    fixef.rm = "none", ssc = ssc_full
  )
  if (!is.null(case$weights)) ref_args$weights = stats::reformulate(case$weights)
  ref = suppressMessages(do.call(fixest::feols, ref_args))
  ref_names = sub("^fit_", "", names(coef(ref)))
  for (batch in c(1L, 3L)) {
    window = fit_with("window", batch, case$f, table, case$vcov, case$weights, ivreg)
    label = paste0(case$name, ", batch ", batch)
    # vcov dimnames can differ from coef names (interactions), so index by position.
    idx = match(ref_names, names(coef(window)))
    expect_true(max(abs(coef(window) - coef(join))) < 1e-9, info = paste("window vs join coef:", label))
    expect_true(max(abs(vcov(window) - vcov(join))) < 1e-9, info = paste("window vs join vcov:", label))
    expect_true(max(abs(coef(window)[idx] - coef(ref))) < 1e-7, info = paste("window vs fixest coef:", label))
    expect_true(max(abs(vcov(window)[idx, idx] - unclass(vcov(ref)))) < 1e-7,
                info = paste("window vs fixest vcov:", label))
  }
}

# Batching: checks after each batch, final partial batch honours max_iter ----
options(dbreg.ap_method = "window", dbreg.ap_batch_size = 6L, dbreg.ap_max_iter = 14L, dbreg.ap_tol = 1e-30)
iters = character(0)
expect_error(withCallingHandlers(
  dbreg(y ~ x1 | fe1 + fe2, conn = con, table = "fixture", strategy = "demean", verbose = TRUE),
  message = function(m) {
    if (grepl("[AP] iter", conditionMessage(m), fixed = TRUE)) {
      iters <<- c(iters, sub(".*iter ([0-9]+):.*", "\\1", conditionMessage(m)))
    }
  }
), "did not converge within 14 iterations")
expect_equal(iters, c("6", "12", "14"), info = "Convergence checked after cycles 6, 12 and 14")
expect_equal(sort(DBI::dbListTables(con)), c("decimal_fixture", "fixture"), info = "Tables dropped on failure")

# Validation ----
options(dbreg.ap_max_iter = 100L, dbreg.ap_tol = 1e-9)
for (bad in list(0L, 1.5, NA_integer_, "6")) {
  options(dbreg.ap_batch_size = bad)
  expect_error(dbreg(y ~ x1 | fe1 + fe2, conn = con, table = "fixture", strategy = "demean"),
               "dbreg.ap_batch_size must be a positive integer", info = paste("batch", format(bad)))
}
options(dbreg.ap_method = "join") # batch size is irrelevant for joins
expect_silent(dbreg(y ~ x1 | fe1 + fe2, conn = con, table = "fixture", strategy = "demean"))

expect_equal(sort(DBI::dbListTables(con)), c("decimal_fixture", "fixture"), info = "Tables dropped after fits")
options(old)
DBI::dbDisconnect(con, shutdown = TRUE)
