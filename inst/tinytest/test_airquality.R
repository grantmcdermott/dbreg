library(fixest)
library(dbreg)

# Load test data
data("airquality")

# dictionary for easy testing across results and methods
dict_db = c("estimate", "std.error", "statistic", "p.values")
names(dict_db) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Simple regression without fixed effects

aq_dbreg = dbreg(Temp ~ Wind, data = airquality)$coeftable
aq_lm = coef(summary(lm(Temp ~ Wind, data = airquality)))

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_dbreg[, dict_db[i]],
    aq_lm[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}

# Single fixed effect (demean strategy)

aq_fe1_demean = dbreg(
  Temp ~ Wind | Month,
  data = airquality,
  strategy = "demean"
)$coeftable

aq_fe1_feols = feols(
  Temp ~ Wind | Month,
  data = airquality,
  lean = TRUE,
  vcov = "iid"
)$coeftable

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_fe1_demean[, dict_db[i]],
    aq_fe1_feols[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}

# Test 'within' alias for demean

aq_fe1_within = dbreg(
  Temp ~ Wind | Month,
  data = airquality,
  strategy = "within"
)$coeftable

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_fe1_within[, dict_db[i]],
    aq_fe1_feols[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}


# Multiple regressors

aq_dbreg_multi = dbreg(
  Temp ~ Wind + Ozone,
  data = airquality
)$coeftable
aq_lm_multi = coef(summary(lm(
  Temp ~ Wind + Ozone,
  data = airquality
)))

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_dbreg_multi[, dict_db[i]],
    aq_lm_multi[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}
