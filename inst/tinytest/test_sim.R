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
  data = sim_panel,
  verbose = FALSE
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
  strategy = "mundlak",
  verbose = FALSE
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
  strategy = "moments",
  verbose = FALSE
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

