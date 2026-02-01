library(dbreg)
library(fixest)

set.seed(123)

# Simulate balanced panel with weights
n_units = 20L
n_time = 4L
unit_fe = rnorm(n_units)
time_fe = rnorm(n_time)

dat = expand.grid(unit = 1:n_units, time = 1:n_time)
dat$fe1 = factor(dat$unit)
dat$fe2 = factor(dat$time)
dat$x1 = rnorm(nrow(dat))
dat$x2 = rnorm(nrow(dat))
dat$weights = runif(nrow(dat), min = 0.5, max = 2.0)
dat$y = unit_fe[dat$unit] + time_fe[dat$time] + 0.5 * dat$x1 - 0.3 * dat$x2 + rnorm(nrow(dat), sd = 0.5)

tol = 1e-6

## No FE: moments vs lm (weights, iid)
lm_fit = lm(y ~ x1 + x2, data = dat, weights = weights)
db_mom = dbreg(y ~ x1 + x2, data = dat, weights = "weights", strategy = "moments", vcov = "iid")

lm_coefs = coef(lm_fit)
db_coefs = db_mom$coeftable[names(lm_coefs), "estimate"]
expect_true(max(abs(lm_coefs - db_coefs)) < tol, info = "moments: weighted coefficients match lm")

lm_ses = summary(lm_fit)$coefficients[, "Std. Error"]
db_ses = db_mom$coeftable[names(lm_ses), "std.error"]
expect_true(max(abs(lm_ses - db_ses)) < tol, info = "moments: weighted SEs match lm")

## One FE: demean vs feols (weights, iid)
fe1_fit = feols(y ~ x1 + x2 | fe1, data = dat, weights = ~weights, vcov = "iid")
db_fe1 = dbreg(y ~ x1 + x2 | fe1, data = dat, weights = "weights", strategy = "demean", vcov = "iid")

fe1_coefs = coef(fe1_fit)
db1_coefs = db_fe1$coeftable[names(fe1_coefs), "estimate"]
expect_true(max(abs(fe1_coefs - db1_coefs)) < tol, info = "demean: weighted coefficients match feols (1 FE)")

fe1_ses = se(fe1_fit)
db1_ses = db_fe1$coeftable[names(fe1_ses), "std.error"]
expect_true(max(abs(fe1_ses - db1_ses)) < tol, info = "demean: weighted SEs match feols (1 FE)")

## Two FE: compress vs feols (weights, iid)
fe2_fit = feols(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = ~weights, vcov = "iid")
db_fe2 = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "compress", vcov = "iid")

fe2_coefs = coef(fe2_fit)
db2_coefs = db_fe2$coeftable[names(fe2_coefs), "estimate"]
expect_true(max(abs(fe2_coefs - db2_coefs)) < tol, info = "compress: weighted coefficients match feols (2 FE)")

fe2_ses = se(fe2_fit)
db2_ses = db_fe2$coeftable[names(fe2_ses), "std.error"]
expect_true(max(abs(fe2_ses - db2_ses)) < tol, info = "compress: weighted SEs match feols (2 FE)")

## Auto: weighted 2 FE should choose compress
db_auto = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "auto", vcov = "iid")
expect_true(db_auto$strategy == "compress", info = "auto: weighted 2 FE selects compress")

## Error cases
expect_error(
  dbreg(y ~ x1 + x2, data = dat, weights = "weights", vcov = "hc1"),
  "vcov"
)
expect_error(
  dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "demean", vcov = "iid"),
  "two-way"
)
