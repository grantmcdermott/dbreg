library(dbreg)
library(fixest)

set.seed(123)

## ---- data ----------------------------------------------------------------
n_units = 20L
n_time = 4L
unit_fe = rnorm(n_units)
time_fe = rnorm(n_time)

dat = expand.grid(unit = 1:n_units, time = 1:n_time)
dat$fe1 = factor(dat$unit)
dat$fe2 = factor(dat$time)
dat$cluster = factor(sample(1:6, nrow(dat), replace = TRUE))
dat$x1 = rnorm(nrow(dat))
dat$x2 = rnorm(nrow(dat))
dat$weights = runif(nrow(dat), min = 0.3, max = 2.5)
dat$y = unit_fe[dat$unit] + time_fe[dat$time] + 0.5 * dat$x1 - 0.3 * dat$x2 + rnorm(nrow(dat), sd = 0.5)

tol_iid = 1e-6
tol_robust = 1e-5

## ---- iid -----------------------------------------------------------------
lm_fit = lm(y ~ x1 + x2, data = dat, weights = weights)
db_mom = dbreg(y ~ x1 + x2, data = dat, weights = "weights", strategy = "moments", vcov = "iid")

lm_coefs = coef(lm_fit)
db_coefs = db_mom$coeftable[names(lm_coefs), "estimate"]
expect_true(max(abs(lm_coefs - db_coefs)) < tol_iid, info = "moments: weighted coefficients match lm (iid)")

lm_ses = summary(lm_fit)$coefficients[, "Std. Error"]
db_ses = db_mom$coeftable[names(lm_ses), "std.error"]
expect_true(max(abs(lm_ses - db_ses)) < tol_iid, info = "moments: weighted SEs match lm (iid)")

fe1_fit = feols(y ~ x1 + x2 | fe1, data = dat, weights = ~weights, vcov = "iid")
db_fe1 = dbreg(y ~ x1 + x2 | fe1, data = dat, weights = "weights", strategy = "demean", vcov = "iid")

fe1_coefs = coef(fe1_fit)
db1_coefs = db_fe1$coeftable[names(fe1_coefs), "estimate"]
expect_true(max(abs(fe1_coefs - db1_coefs)) < tol_iid, info = "demean: weighted coefficients match feols (iid)")

fe1_ses = se(fe1_fit)
db1_ses = db_fe1$coeftable[names(fe1_ses), "std.error"]
expect_true(max(abs(fe1_ses - db1_ses)) < tol_iid, info = "demean: weighted SEs match feols (iid)")

fe2_fit = feols(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = ~weights, vcov = "iid")
db_fe2 = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "compress", vcov = "iid")

fe2_coefs = coef(fe2_fit)
db2_coefs = db_fe2$coeftable[names(fe2_coefs), "estimate"]
expect_true(max(abs(fe2_coefs - db2_coefs)) < tol_iid, info = "compress: weighted coefficients match feols (iid)")

fe2_ses = se(fe2_fit)
db2_ses = db_fe2$coeftable[names(fe2_ses), "std.error"]
expect_true(max(abs(fe2_ses - db2_ses)) < tol_iid, info = "compress: weighted SEs match feols (iid)")

## ---- hc1 -----------------------------------------------------------------
fe_mom_hc1 = feols(y ~ x1 + x2, data = dat, weights = ~weights, vcov = "hc1")
db_mom_hc1 = dbreg(y ~ x1 + x2, data = dat, weights = "weights", strategy = "moments", vcov = "hc1")
expect_true(max(abs(se(fe_mom_hc1) - db_mom_hc1$coeftable[names(se(fe_mom_hc1)), "std.error"])) < tol_robust,
            info = "moments: weighted HC1 SEs match feols")

fe1_hc1 = feols(y ~ x1 + x2 | fe1, data = dat, weights = ~weights, vcov = "hc1")
db_fe1_hc1 = dbreg(y ~ x1 + x2 | fe1, data = dat, weights = "weights", strategy = "demean", vcov = "hc1")
expect_true(max(abs(se(fe1_hc1) - db_fe1_hc1$coeftable[names(se(fe1_hc1)), "std.error"])) < tol_robust,
            info = "demean: weighted HC1 SEs match feols")

fe2_hc1 = feols(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = ~weights, vcov = "hc1")
db_fe2_hc1 = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "compress", vcov = "hc1")
expect_true(max(abs(se(fe2_hc1) - db_fe2_hc1$coeftable[names(se(fe2_hc1)), "std.error"])) < tol_robust,
            info = "compress: weighted HC1 SEs match feols")

## ---- cluster --------------------------------------------------------------
fe_mom_cl = feols(y ~ x1 + x2, data = dat, weights = ~weights, vcov = ~cluster)
db_mom_cl = dbreg(y ~ x1 + x2, data = dat, weights = "weights", strategy = "moments", vcov = ~cluster)
expect_true(max(abs(se(fe_mom_cl) - db_mom_cl$coeftable[names(se(fe_mom_cl)), "std.error"])) < tol_robust,
            info = "moments: weighted cluster SEs match feols")

fe1_cl = feols(y ~ x1 + x2 | fe1, data = dat, weights = ~weights, vcov = ~cluster)
db_fe1_cl = dbreg(y ~ x1 + x2 | fe1, data = dat, weights = "weights", strategy = "demean", vcov = ~cluster)
expect_true(max(abs(se(fe1_cl) - db_fe1_cl$coeftable[names(se(fe1_cl)), "std.error"])) < tol_robust,
            info = "demean: weighted cluster SEs match feols")

fe2_cl = feols(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = ~weights, vcov = ~cluster)
db_fe2_cl = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "compress", vcov = ~cluster)
expect_true(max(abs(se(fe2_cl) - db_fe2_cl$coeftable[names(se(fe2_cl)), "std.error"])) < tol_robust,
            info = "compress: weighted cluster SEs match feols")

## ---- weights==1 -----------------------------------------------------------
dat$w1 = 1
w1_mom = dbreg(y ~ x1 + x2, data = dat, weights = "w1", strategy = "moments", vcov = "iid")
unw_mom = dbreg(y ~ x1 + x2, data = dat, strategy = "moments", vcov = "iid")
names_mom = rownames(unw_mom$coeftable)
expect_true(max(abs(w1_mom$coeftable[names_mom, "estimate"] - unw_mom$coeftable[names_mom, "estimate"])) < tol_iid,
            info = "weights==1: moments coefficients match unweighted")
expect_true(max(abs(w1_mom$coeftable[names_mom, "std.error"] - unw_mom$coeftable[names_mom, "std.error"])) < tol_iid,
            info = "weights==1: moments SEs match unweighted")

## ---- unbalanced -----------------------------------------------------------
set.seed(321)
drop_idx = sample(seq_len(nrow(dat)), size = round(0.2 * nrow(dat)))
dat_unbal = dat[-drop_idx, ]
fe2_unbal = feols(y ~ x1 + x2 | fe1 + fe2, data = dat_unbal, weights = ~weights, vcov = "iid")
db_unbal = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat_unbal, weights = "weights", strategy = "compress", vcov = "iid")

unbal_coefs = coef(fe2_unbal)
db_unbal_coefs = db_unbal$coeftable[names(unbal_coefs), "estimate"]
expect_true(max(abs(unbal_coefs - db_unbal_coefs)) < tol_iid, info = "unbalanced: coefficients match feols (compress)")

unbal_ses = se(fe2_unbal)
db_unbal_ses = db_unbal$coeftable[names(unbal_ses), "std.error"]
expect_true(max(abs(unbal_ses - db_unbal_ses)) < tol_iid, info = "unbalanced: SEs match feols (compress)")

## ---- auto ----------------------------------------------------------------
expect_true(
  dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "auto", vcov = "iid")$strategy == "demean",
  info = "auto: weighted 2 FE selects demean (AP)"
)

## ---- zero-weight + negative ------------------------------------------------
set.seed(126)
zero_idx = sample(seq_len(nrow(dat)), size = 10)
dat_zero = dat
dat_zero$weights[zero_idx] = 0

fe_zero = feols(y ~ x1 + x2 | fe1, data = dat_zero[dat_zero$weights > 0, ], weights = ~weights, vcov = "iid")
db_zero = dbreg(y ~ x1 + x2 | fe1, data = dat_zero, weights = "weights", strategy = "demean", vcov = "iid")

zero_coefs = coef(fe_zero)
db_zero_coefs = db_zero$coeftable[names(zero_coefs), "estimate"]
expect_true(max(abs(zero_coefs - db_zero_coefs)) < tol_iid, info = "zero weights dropped: coefficients match")

zero_ses = se(fe_zero)
db_zero_ses = db_zero$coeftable[names(zero_ses), "std.error"]
expect_true(max(abs(zero_ses - db_zero_ses)) < tol_iid, info = "zero weights dropped: SEs match")

bad = dat
bad$weights[1] = -1
expect_error(
  dbreg(y ~ x1 + x2 | fe1, data = bad, weights = "weights", strategy = "demean"),
  "non-negative"
)

## ---- ap-smoke --------------------------------------------------------------
db_ap = dbreg(y ~ x1 + x2 | fe1 + fe2, data = dat, weights = "weights", strategy = "demean", vcov = "iid")
expect_true(db_ap$strategy == "demean", info = "weighted 2 FE demean runs via AP")
