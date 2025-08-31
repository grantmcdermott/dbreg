## Run all strategies on a simple dataset and compare to feols

# Dependencies
library(fixest)
library(data.table)

# True coefficients
beta_true = c(x1 = 0.5, x2 = -0.3)

## -------------------------------------
## Functions 
## -------------------------------------
# Modified data generation function (simplified from benchmark.R)
gen_dat = function(
  N_units = 100,
  T_periods = 5L,
  beta = beta_true,
  K1 = 4L,
  K2 = 4L,
  sd_fe_unit = 1.0,
  sd_fe_time = 0.5,
  sd_err = 0.8,
  add_weights = TRUE
) {
  unit_fe = rnorm(N_units, sd = sd_fe_unit)
  time_fe = rnorm(T_periods, sd = sd_fe_time)

  dat = CJ(unit = 1:N_units, time = 1:T_periods)

  # Create latent variables and discretize
  dat[, let(
    latent_x1 = 0.6 * unit_fe[unit] + 0.3 * time_fe[time] + rnorm(.N, sd = 0.7),
    latent_x2 = -0.4 * unit_fe[unit] + 0.5 * time_fe[time] + rnorm(.N, sd = 0.7)
  )]

  # Compute quantile cuts
  cuts1 = quantile(dat$latent_x1, probs = seq(0, 1, length.out = K1 + 1))
  cuts2 = quantile(dat$latent_x2, probs = seq(0, 1, length.out = K2 + 1))

  # Make unique breakpoints
  cuts1 = unique(cuts1)
  while (length(cuts1) < K1 + 1) {
    cuts1 = sort(unique(c(cuts1, jitter(cuts1[length(cuts1)], 1e-6))))
  }
  cuts2 = unique(cuts2)
  while (length(cuts2) < K2 + 1) {
    cuts2 = sort(unique(c(cuts2, jitter(cuts2[length(cuts2)], 1e-6))))
  }

  dat[, let(
    x1 = as.integer(cut(
      latent_x1,
      breaks = cuts1,
      include.lowest = TRUE,
      labels = FALSE
    )) - 1L,
    x2 = as.integer(cut(
      latent_x2,
      breaks = cuts2,
      include.lowest = TRUE,
      labels = FALSE
    )) - 1L
  )][,
    y := unit_fe[unit] +
      time_fe[time] +
      beta["x1"] * x1 +
      beta["x2"] * x2 +
      rnorm(.N, sd = sd_err)
  ]

  # Add weights if requested
  if (add_weights) {
    dat[, weights := runif(.N, min = 0.5, max = 2.0)]
  }

  dat[, c("latent_x1", "latent_x2") := NULL]
  setnames(dat, c("unit", "time"), c("unit_fe", "time_fe"))

  return(dat)
}

# Test function to compare results (silent, returns data.table)
run_all_strategies = function(
    data, 
    formula_str, 
    weights_col = NULL, 
    test_name = "") {
  # Parse formula
  fml = as.formula(formula_str)
  
  # Fit with feols
  if (!is.null(weights_col)) {
    feols_fit = feols(fml, data = data, weights = data[[weights_col]], vcov = "iid")
  } else {
    feols_fit = feols(fml, data = data, vcov = "iid")
  }
  
  # Fit with dbreg (test all strategies)
  strategies = c("moments", "compress", "mundlak")
  
  results = list()
  
  # Add feols results
  feols_coefs = coef(feols_fit)
  feols_ses = se(feols_fit)
  
  results[["feols"]] = data.table(
    test = test_name,
    method = "feols",
    intercept = ifelse("(Intercept)" %in% names(feols_coefs), feols_coefs["(Intercept)"], NA_real_),
    x1 = feols_coefs["x1"],
    x2 = feols_coefs["x2"],
    intercept_se = ifelse("(Intercept)" %in% names(feols_ses), feols_ses["(Intercept)"], NA_real_),
    x1_se = feols_ses["x1"],
    x2_se = feols_ses["x2"]
  )
  
  # Add dbreg results for each strategy
  for (strategy in strategies) {
      if (!is.null(weights_col)) {
        dbreg_fit = dbreg(
          fml = fml, 
          data = data, 
          weights = weights_col,
          strategy = strategy,
          vcov = "iid"
        )
      } else {
        dbreg_fit = dbreg(
          fml = fml, 
          data = data, 
          strategy = strategy,
          vcov = "iid"
        )
      }
      
      dbreg_coefs = dbreg_fit$coeftable[, "estimate"]
      dbreg_ses = dbreg_fit$coeftable[, "std.error"]
      
      results[[strategy]] = data.table(
        test = test_name,
        method = strategy,
        intercept = ifelse("(Intercept)" %in% names(dbreg_coefs), dbreg_coefs["(Intercept)"], NA_real_),
        x1 = dbreg_coefs["x1"],
        x2 = dbreg_coefs["x2"],
        intercept_se = ifelse("(Intercept)" %in% names(dbreg_ses), dbreg_ses["(Intercept)"], NA_real_),
        x1_se = dbreg_ses["x1"],
        x2_se = dbreg_ses["x2"]
      )
  }
  
  return(rbindlist(results))
}


# Make data
dat1 = gen_dat(N_units = 50, T_periods = 3, add_weights = TRUE)

## -------------------------------------
## Run tests
## -------------------------------------
all_results = list()
# Test 1: Simple regression with weights
all_results[["test1"]] = run_all_strategies(dat1, "y ~ x1 + x2", "weights", "Weighted (no FE)")

# Test 2: Simple regression without weights
all_results[["test2"]] = run_all_strategies(dat1, "y ~ x1 + x2", NULL, "Unweighted (no FE)")

# Test 3: Single FE with weights
all_results[["test3"]] = run_all_strategies(dat1, "y ~ x1 + x2 | unit_fe", "weights", "Weighted (1 FE)")

# Test 4: Double FE with weights
all_results[["test4"]] = run_all_strategies(dat1, "y ~ x1 + x2 | unit_fe + time_fe", "weights", "Weighted (2 FE)")


# Combine all results
combined_results = rbindlist(all_results)

# Pivot longer so that x1, x2, and all se's are in long format
library(tidyr)
# Pivot coefficients and SEs into two long tables
coefficients_long = pivot_longer(
    combined_results %>% select(-intercept_se, -x1_se, -x2_se),
    cols = c("intercept", "x1", "x2"),
    names_to = "variable",
    values_to = "estimate"
)

ses_long = pivot_longer(
    combined_results %>% select(-intercept, -x1, -x2),
    cols = c("intercept_se", "x1_se", "x2_se"),
    names_to = "variable",
    values_to = "std.error"
)

# Formal tinytest checks comparing dbreg strategies to feols
tol = 1e-10
strategies = c("moments", "compress", "mundlak")

for (test_name in unique(combined_results$test)) {
    fe_row = combined_results[test == test_name & method == "feols"][1]
    for (strategy in strategies) {
        dr_row = combined_results[test == test_name & method == strategy][1]
        tinytest::expect_true(
            abs(as.numeric(fe_row$x1) - as.numeric(dr_row$x1)) < tol,
            paste0("Coef x1 mismatch: ", test_name, " - ", strategy)
        )
        tinytest::expect_true(
            abs(as.numeric(fe_row$x2) - as.numeric(dr_row$x2)) < tol,
            paste0("Coef x2 mismatch: ", test_name, " - ", strategy)
        )
        tinytest::expect_true(
            abs(as.numeric(fe_row$x1_se) - as.numeric(dr_row$x1_se)) < tol,
            paste0("SE x1 mismatch: ", test_name, " - ", strategy)
        )
        tinytest::expect_true(
            abs(as.numeric(fe_row$x2_se) - as.numeric(dr_row$x2_se)) < tol,
            paste0("SE x2 mismatch: ", test_name, " - ", strategy)
        )
        tinytest::expect_true(
            is.numeric(dr_row$intercept) || is.na(dr_row$intercept),
            paste0("Intercept type unexpected: ", test_name, " - ", strategy)
        )
    }
}

# Additional sanity check(s): tests expected
tinytest::expect_true(all(c("feols", strategies) %in% unique(combined_results$method)), "Missing methods in results")
