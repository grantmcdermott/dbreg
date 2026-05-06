library(dbreg)
library(fixest)

tol = 1e-6

## ---- 3 FE ----------------------------------------------------------------
set.seed(42)
dat3 = expand.grid(id = 1:8, time = 1:4, firm = 1:5)
firm_shock = rnorm(5)
dat3$x = 0.9 * firm_shock[dat3$firm] + rnorm(nrow(dat3))
dat3$y = 1.2 * dat3$x +
  rnorm(8)[dat3$id] +
  rnorm(4)[dat3$time] +
  3 * firm_shock[dat3$firm] +
  rnorm(nrow(dat3), sd = 0.3)

db3 = dbreg(y ~ x | id + time + firm, data = dat3, strategy = "demean", vcov = "iid")
fe3 = feols(y ~ x | id + time + firm, data = dat3, vcov = "iid")
fe2 = feols(y ~ x | id + time, data = dat3, vcov = "iid")

expect_true(db3$strategy == "demean" && identical(db3$demean_method, "ap"),
            info = "3 FE explicit demean uses alternating projections")
expect_true(abs(coef(fe2)["x"] - coef(fe3)["x"]) > 0.1,
            info = "3 FE test data would fail if third FE were ignored")
expect_equal(db3$coeftable["x", "estimate"], unname(coef(fe3)["x"]), tolerance = tol,
             info = "3 FE AP coefficient matches feols")
expect_equal(db3$coeftable["x", "std.error"], unname(se(fe3)["x"]), tolerance = tol,
             info = "3 FE AP IID SE matches feols")
expect_true(grepl("Alternating-projection", capture.output(print(db3))[1], fixed = TRUE),
            info = "3 FE AP print method identifies alternating projections")
expect_error(predict(db3, newdata = dat3), "more than two fixed effects")

## ---- 4 FE ----------------------------------------------------------------
set.seed(43)
dat4 = expand.grid(fe1 = 1:5, fe2 = 1:4, fe3 = 1:3, fe4 = 1:2)
dat4$x = rnorm(nrow(dat4))
dat4$y = -0.7 * dat4$x +
  rnorm(5)[dat4$fe1] +
  rnorm(4)[dat4$fe2] +
  rnorm(3)[dat4$fe3] +
  rnorm(2)[dat4$fe4] +
  rnorm(nrow(dat4), sd = 0.4)

db4 = dbreg(y ~ x | fe1 + fe2 + fe3 + fe4, data = dat4, strategy = "demean", vcov = "iid")
fe4 = feols(y ~ x | fe1 + fe2 + fe3 + fe4, data = dat4, vcov = "iid")

expect_equal(db4$coeftable["x", "estimate"], unname(coef(fe4)["x"]), tolerance = tol,
             info = "4 FE AP coefficient matches feols")
expect_equal(db4$coeftable["x", "std.error"], unname(se(fe4)["x"]), tolerance = tol,
             info = "4 FE AP IID SE matches feols")
