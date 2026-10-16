library(dbreg)

# Alternating projections (AP) on the default join implementation. Random,
# unbalanced two-way FE route `strategy = "demean"` through AP.
set.seed(42)
n = 1500L
dat = data.frame(
  fe1 = sample.int(30, n, TRUE), fe2 = sample.int(20, n, TRUE),
  fe3 = sample.int(8, n, TRUE), x1 = rnorm(n)
)
dat$y = 0.5 * dat$x1 + sin(dat$fe1) + cos(dat$fe2) + dat$fe3 / 10 + rnorm(n)
old = options(dbreg.ap_tol = 1e-10, dbreg.ap_max_iter = 500L)
ssc_full = fixest::ssc(K.fixef = "full")
expect_message(
  dbreg(y ~ x1 | fe1 + fe2, data = dat, strategy = "demean", verbose = TRUE),
  "alternating projections", info = "Fixture exercises AP"
)

# Cluster identifier that is also a regressor (previously an SQL binder error).
fit = dbreg(y ~ x1 + fe3 | fe1 + fe2, data = dat, strategy = "demean", vcov = ~fe3)
ref = fixest::feols(y ~ x1 + fe3 | fe1 + fe2, dat, vcov = ~fe3, fixef.rm = "none", ssc = ssc_full)
expect_equal(coef(fit), coef(ref), tolerance = 1e-7, info = "AP: cluster var in RHS (coef)")
expect_true(max(abs(vcov(fit) - vcov(ref))) < 1e-7, info = "AP: cluster var in RHS (vcov)")

# FE identifier that is also a regressor: absorbed as collinear, not an SQL error.
fit = dbreg(y ~ x1 + fe1 | fe1 + fe2, data = dat, strategy = "demean", vcov = "hc1")
ref = suppressMessages(fixest::feols(
  y ~ x1 + fe1 | fe1 + fe2, dat, vcov = "hetero", fixef.rm = "none", ssc = ssc_full
))
expect_equal(coef(fit)[["x1"]], coef(ref)[["x1"]], tolerance = 1e-7, info = "AP: FE var in RHS (coef)")
expect_equal(vcov(fit)["x1", "x1"], vcov(ref)["x1", "x1"], tolerance = 1e-7, info = "AP: FE var in RHS (vcov)")

# Invalid controls fail clearly instead of with internal errors.
for (bad in list(0L, -1L, 2.5, NA_integer_, "10", c(5L, 6L))) {
  options(dbreg.ap_max_iter = bad)
  expect_error(dbreg(y ~ x1 | fe1 + fe2, data = dat, strategy = "demean"),
               "dbreg.ap_max_iter must be a positive integer", info = paste("max_iter", format(bad)))
}
options(dbreg.ap_max_iter = 500L)
for (bad in list(0, -1e-8, NA_real_, Inf)) {
  options(dbreg.ap_tol = bad)
  expect_error(dbreg(y ~ x1 | fe1 + fe2, data = dat, strategy = "demean"),
               "dbreg.ap_tol must be a positive number", info = paste("tol", format(bad)))
}

# Non-convergence reports the limit and leaves no AP tables behind.
con = DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "dat", dat)
options(dbreg.ap_tol = 1e-30, dbreg.ap_max_iter = 3L)
expect_error(dbreg(y ~ x1 | fe1 + fe2, conn = con, table = "dat", strategy = "demean"),
             "did not converge within 3 iterations")
expect_equal(DBI::dbListTables(con), "dat", info = "AP tables dropped after non-convergence")
DBI::dbDisconnect(con, shutdown = TRUE)

options(old)
