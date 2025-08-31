library(fixest)
library(dbreg)

# Load test data
data("airquality")

# dictionary for easy testing across results and methods
dict_db = c("estimate", "std.error", "statistic", "p.values")
names(dict_db) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Simple regression without fixed effects

aq_dbreg = dbreg(Temp ~ Wind, data = airquality, verbose = FALSE)$coeftable
aq_lm = coef(summary(lm(Temp ~ Wind, data = airquality)))

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_dbreg[, dict_db[i]],
    aq_lm[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}

# Single fixed effect (mundlak strategy)

aq_fe1_mundlak = dbreg(
  Temp ~ Wind | Month,
  data = airquality,
  strategy = "mundlak",
  verbose = FALSE
)$coeftable

aq_fe1_feols = feols(
  Temp ~ Wind | Month,
  data = airquality,
  lean = TRUE,
  vcov = "iid"
)$coeftable

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_fe1_mundlak[, dict_db[i]],
    aq_fe1_feols[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}


# Multiple regressors

aq_complete = airquality[
  complete.cases(airquality[, c("Temp", "Wind", "Ozone")]),
]
aq_dbreg_multi = dbreg(
  Temp ~ Wind + Ozone,
  data = aq_complete,
  verbose = FALSE
)$coeftable
aq_lm_multi = coef(summary(lm(
  Temp ~ Wind + Ozone,
  data = aq_complete
)))

for (i in seq_along(dict_db)) {
  expect_equal(
    aq_dbreg_multi[, dict_db[i]],
    aq_lm_multi[, names(dict_db)[i]],
    tolerance = 1e-6
  )
}
