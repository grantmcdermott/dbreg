library(duckdb)
library(dbreg)

# Simulate staggered treatment panel data
sim_event = local({
  set.seed(42)
  
  n_units = 200
  n_periods = 10
  
  panel = expand.grid(
    unit_id = 1:n_units,
    time = 1:n_periods
  )
  
  # Assign treatment timing (staggered adoption)
  # 30% never treated, rest treated at different times
  treatment_timing = data.frame(
    unit_id = 1:n_units,
    treat_time = sample(
      c(NA, 4:7), n_units, replace = TRUE, 
      prob = c(0.3, rep(0.7/4, 4))
    )
  )
  
  panel = merge(panel, treatment_timing, by = "unit_id")
  
  # Create treatment indicator (absorbing)
  panel$treat = with(panel, ifelse(is.na(treat_time), 0, as.integer(time >= treat_time)))
  
  # Create outcome with unit + time FE + dynamic treatment effects
  unit_fe = rnorm(n_units, 0, 2)
  time_fe = rnorm(n_periods, 0, 1)
  
  # True treatment effects by event time
  get_true_effect = function(event_time) {
    ifelse(is.na(event_time), 0,
      ifelse(event_time < -2, 0,
      ifelse(event_time == -2, 0.2,
      ifelse(event_time == -1, 0.5,
      ifelse(event_time == 0, 1.0,
      ifelse(event_time == 1, 1.5,
      ifelse(event_time == 2, 2.0, 2.5)))))))
  }
  
  # Compute event time and outcome
  panel$event_time_true = with(panel, ifelse(is.na(treat_time), NA, time - treat_time))
  panel$true_effect = get_true_effect(panel$event_time_true)
  
  panel$y = unit_fe[panel$unit_id] + 
            time_fe[panel$time] + 
            panel$true_effect + 
            rnorm(nrow(panel), 0, 0.5)
  
  # Add a covariate for testing
  panel$x = rnorm(nrow(panel))
  
  # Remove helper columns
  panel$event_time_true = NULL
  panel$true_effect = NULL
  panel$treat_time = NULL
  
  panel
})

# Connect to DuckDB
con = dbConnect(duckdb())
dbWriteTable(con, "panel_data", sim_event, overwrite = TRUE)

#
## Test basic event study ----

es_basic = dbevent(
  outcome = "y",
  conn = con,
  table = "panel_data",
  treat = "treat",
  unit = "unit_id",
  time = "time",
  leads = 3,
  lags = 4,
  ref = -1
)

expect_inherits(
  es_basic, "dbevent",
  info = "dbevent returns dbevent class"
)

expect_true(
  !is.null(es_basic$coefs),
  info = "dbevent has coefs"
)

expect_true(
  nrow(es_basic$coefs) > 0,
  info = "dbevent coefs has rows"
)

expect_equal(
  es_basic$ref, -1L,
  info = "reference period stored correctly"
)

#
## Test coefficient extraction ----

coefs = coef(es_basic)

expect_true(
  is.numeric(coefs),
  info = "coef.dbevent returns numeric"
)

expect_true(
  length(coefs) > 0,
  info = "coef.dbevent returns coefficients"
)

#
## Test auto-detect leads/lags ----

es_auto = dbevent(
  outcome = "y",
  conn = con,
  table = "panel_data",
  treat = "treat",
  unit = "unit_id",
  time = "time"
)

expect_inherits(
  es_auto, "dbevent",
  info = "auto-detect leads/lags works"
)

expect_true(
  !is.null(es_auto$event_time$leads) && !is.null(es_auto$event_time$lags),
  info = "leads and lags auto-detected"
)

#
## Test with covariates ----

es_cov = dbevent(
  outcome = "y",
  conn = con,
  table = "panel_data",
  treat = "treat",
  unit = "unit_id",
  time = "time",
  covariates = "x",
  leads = 2,
  lags = 3
)

expect_inherits(
  es_cov, "dbevent",
  info = "dbevent with covariates works"
)

#
## Test validation: non-binary treatment ----

sim_bad = sim_event
sim_bad$treat = sim_bad$treat * 2  # Values are now 0, 2
dbWriteTable(con, "panel_bad", sim_bad, overwrite = TRUE)

expect_error(
  dbevent(
    outcome = "y",
    conn = con,
    table = "panel_bad",
    treat = "treat",
    unit = "unit_id",
    time = "time"
  ),
  pattern = "binary",
  info = "non-binary treatment throws error"
)

#
## Test print method ----

expect_silent(
  capture.output(print(es_basic)),
  info = "print.dbevent works without error"
)

#
## Test summary method ----

summ = summary(es_basic)

expect_inherits(
  summ, "dbevent",
  info = "summary.dbevent returns dbevent object"
)

# Cleanup
dbDisconnect(con)
