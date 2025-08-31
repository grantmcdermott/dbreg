## Run all strategies on a simple dataset and compare to feols

# Dependencies
library(fixest)
library(data.table)
library(tidyr)

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
  sd_fe_unit = 1.0,
  sd_fe_time = 0.5,
  sd_err = 0.8,
  add_weights = TRUE
) {
  # Generate fixed effects
  unit_fe = rnorm(N_units, sd = sd_fe_unit)
  time_fe = rnorm(T_periods, sd = sd_fe_time)

  # Create panel structure
  dat = CJ(unit_fe = 1:N_units, time_fe = 1:T_periods)
  
  # Generate continuous x variables directly 
  dat[, `:=`(
    x1 = 0.6 * unit_fe[unit_fe] + 0.3 * time_fe[time_fe] + rnorm(.N, sd = 0.7),
    x2 = -0.4 * unit_fe[unit_fe] + 0.5 * time_fe[time_fe] + rnorm(.N, sd = 0.7)
  )]
  
  # Generate outcome
  dat[, y := unit_fe[unit_fe] +
      time_fe[time_fe] +
      beta["x1"] * x1 +
      beta["x2"] * x2 +
      rnorm(.N, sd = sd_err)
  ]

  # Add weights if requested
  if (add_weights) {
    dat[, weights := runif(.N, min = 0.5, max = 2.0)]
  }

  return(dat)
}

# Test function to compare results (silent, returns data.table)
run_all_strategies = function(
    data, 
    formula_str, 
    weights_col = NULL, 
    test_name = "") {

  results = list()
  
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
