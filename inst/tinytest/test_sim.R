library(fixest)
library(dbreg)

# Simulate *balanced* panel: 50 firms × 10 years = 500 obs
sim_panel = local({
  set.seed(123)
  n_firms = 50
  n_years = 10
  n = n_firms * n_years
  
  dat = data.frame(
    firm = rep(1:n_firms, each = n_years),
    year = rep(1:n_years, times = n_firms),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  
  # Add firm and year effects + outcome (IID errors)
  firm_fe = rnorm(n_firms)[dat$firm]
  year_fe = rnorm(n_years)[dat$year]
  dat$y = 2 + 1.5 * dat$x1 - 0.8 * dat$x2 + firm_fe + year_fe + rnorm(n)
  
  dat
})

#
## Test auto strategy (balanced but not compressible, so should select demean) ----

sim_dbreg = dbreg(
  y ~ x1 + x2 | firm + year,
  data = sim_panel
)

# Check auto selected demean
expect_equal(
  sim_dbreg$strategy, "demean",
  info = "balanced panel (sim): auto selects demean"
)

sim_feols = feols(
  y ~ x1 + x2 | firm + year,
  data = sim_panel,
  lean = TRUE
)

expect_equal(
  sim_dbreg$coeftable[c("x1", "x2"), "estimate"],
  sim_feols$coeftable[c("x1", "x2"), "Estimate"],
  tolerance = 1e-6,
  info = "balanced panel (sim): demean coefs match feols"
)
expect_equal(
  sim_dbreg$coeftable[c("x1", "x2"), "std.error"],
  sim_feols$coeftable[c("x1", "x2"), "Std. Error"],
  tolerance = 1e-6,
  info = "balanced panel (sim): demean SEs match feols"
)

#
## Test mundlak strategy ----

# Create group means for manual Mundlak comparison
sim_panel$x1_mean_firm = ave(sim_panel$x1, sim_panel$firm, FUN = mean)
sim_panel$x2_mean_firm = ave(sim_panel$x2, sim_panel$firm, FUN = mean)
sim_panel$x1_mean_year = ave(sim_panel$x1, sim_panel$year, FUN = mean)
sim_panel$x2_mean_year = ave(sim_panel$x2, sim_panel$year, FUN = mean)

sim_mundlak = dbreg(
  y ~ x1 + x2 | firm + year,
  data = sim_panel,
  strategy = "mundlak"
)

sim_mundlak_manual = feols(
  y ~ x1 + x2 + x1_mean_firm + x2_mean_firm + x1_mean_year + x2_mean_year,
  data = sim_panel,
  lean = TRUE
)

expect_equal(
  sim_mundlak$coeftable[c("x1", "x2"), "estimate"],
  sim_mundlak_manual$coeftable[c("x1", "x2"), "Estimate"],
  tolerance = 1e-6,
  info = "balanced panel (sim): mundlak coefs match manual"
)
expect_equal(
  sim_mundlak$coeftable[c("x1", "x2"), "std.error"],
  sim_mundlak_manual$coeftable[c("x1", "x2"), "Std. Error"],
  tolerance = 1e-6,
  info = "balanced panel (sim): mundlak SEs match manual"
)

#
## Test moments strategy (no FE) ----

sim_moments = dbreg(
  y ~ x1 + x2,
  data = sim_panel,
  strategy = "moments"
)

sim_ols = feols(
  y ~ x1 + x2,
  data = sim_panel,
  lean = TRUE
)

expect_equal(
  sim_moments$coeftable[c("(Intercept)", "x1", "x2"), "estimate"],
  sim_ols$coeftable[c("(Intercept)", "x1", "x2"), "Estimate"],
  tolerance = 1e-6,
  info = "balanced panel (sim): moments coefs match feols"
)
expect_equal(
  sim_moments$coeftable[c("(Intercept)", "x1", "x2"), "std.error"],
  sim_ols$coeftable[c("(Intercept)", "x1", "x2"), "Std. Error"],
  tolerance = 1e-6,
  info = "balanced panel (sim): moments SEs match feols"
)


#
## Test predictions (balanced panel) ----

# Need non-lean fixest for predictions
sim_feols_full = feols(
  y ~ x1 + x2 | firm + year,
  data = sim_panel
)

sim_compress = dbreg(
  y ~ x1 + x2 | firm + year,
  data = sim_panel,
  strategy = "compress"
)

# Compress: point predictions match fixest
sim_pred_compress = predict(sim_compress, newdata = sim_panel)
sim_pred_feols = predict(sim_feols_full, newdata = sim_panel)

expect_equal(
  sim_pred_compress,
  as.numeric(sim_pred_feols),
  tolerance = 1e-6,
  info = "balanced panel: compress predictions match fixest"
)

# Demean: point predictions match fixest (balanced panel = exact TWFE)
sim_pred_demean = predict(sim_dbreg, newdata = sim_panel)

expect_equal(
  sim_pred_demean,
  as.numeric(sim_pred_feols),
  tolerance = 1e-6,
  info = "balanced panel: demean predictions match fixest"
)

# Compress: confidence intervals
sim_ci_compress = predict(sim_compress, newdata = sim_panel, interval = "confidence")
expect_true(
  all(sim_ci_compress$lwr < sim_ci_compress$fit & sim_ci_compress$fit < sim_ci_compress$upr),
  info = "balanced panel: compress CI bounds are sensible"
)

# Compress: prediction intervals wider than confidence intervals
sim_pi_compress = predict(sim_compress, newdata = sim_panel, interval = "prediction")
expect_true(
  all((sim_pi_compress$upr - sim_pi_compress$lwr) > (sim_ci_compress$upr - sim_ci_compress$lwr)),
  info = "balanced panel: prediction intervals wider than confidence intervals"
)



#
## Rows with a missing cluster variable are dropped (as in fixest) ----

sim_na_cl = sim_panel
sim_na_cl$cl = (sim_na_cl$firm %% 10) + 1
sim_na_cl$cl[c(3, 57, 211, 404)] = NA
fx_na_cl = feols(y ~ x1 + x2 | firm, data = sim_na_cl[!is.na(sim_na_cl$cl), ], vcov = ~cl)
fx_na_cl_nofe = feols(y ~ x1 + x2, data = sim_na_cl[!is.na(sim_na_cl$cl), ], vcov = ~cl)

for (s in c("demean", "compress")) {
  # firm is nested in cl; fixest's default K excludes nested FEs
  db_na_cl = dbreg(y ~ x1 + x2 | firm, data = sim_na_cl, vcov = ~cl, ssc = "nested", strategy = s)
  expect_equal(db_na_cl$nobs_orig, nobs(fx_na_cl), info = paste(s, "drops rows with missing cluster"))
  expect_equal(
    as.numeric(db_na_cl$coeftable[c("x1", "x2"), "std.error"]),
    as.numeric(se(fx_na_cl)[c("x1", "x2")]),
    tolerance = 1e-6,
    info = paste(s, "cluster SEs match fixest after dropping missing clusters")
  )
}
db_na_cl_mom = dbreg(y ~ x1 + x2, data = sim_na_cl, vcov = ~cl, strategy = "moments")
expect_equal(db_na_cl_mom$nobs_orig, nobs(fx_na_cl_nofe), info = "moments drops rows with missing cluster")
expect_equal(
  as.numeric(db_na_cl_mom$coeftable[, "std.error"]),
  as.numeric(se(fx_na_cl_nofe)),
  tolerance = 1e-6,
  info = "moments cluster SEs match fixest after dropping missing clusters"
)

#
## Moments are computed in double precision (FLOAT is 4-byte on DuckDB) ----

sim_cl = transform(sim_panel, cl = (firm %% 10) + 1)
fx_prec = feols(y ~ x1 + x2 | firm, data = sim_cl, vcov = ~cl)
for (s in c("demean", "mundlak")) {
  db_prec = dbreg(y ~ x1 + x2 | firm, data = sim_cl, vcov = ~cl, strategy = s)
  expect_equal(coef(db_prec)[c("x1", "x2")], coef(fx_prec), tolerance = 1e-12, info = paste(s, "coefficients match fixest to double precision"))
}
db_prec = dbreg(y ~ x1 + x2 | firm, data = sim_cl, vcov = ~cl, ssc = "nested", strategy = "demean")
expect_equal(as.numeric(db_prec$coeftable[c("x1", "x2"), "std.error"]), as.numeric(se(fx_prec)), tolerance = 1e-12, info = "cluster SEs match fixest to double precision")
