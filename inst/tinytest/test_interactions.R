library(fixest)
library(dbreg)

# Create test dataset with outcomes dependent on predictors
set.seed(123)
n = 200
test_df = within(data.frame(
  x1 = rnorm(n),
  x2 = sample(c("a", "b", "c"), n, replace = TRUE),
  x3 = rnorm(n),
  fe = sample(1:5, n, replace = TRUE)
), {
  # y: no FE effect (for testing without FE or with FE absorbed)
  y = 1 + 2*x1 + 3*(x2 == "b") + 4*(x2 == "c") +
      0.5*x1*(x2 == "b") - 1*x1*(x2 == "c") + rnorm(n, sd = 0.5)
  # y1: includes FE effect (numeric × factor interaction)
  y1 = y + fe * 0.5
  # y2: numeric × numeric interaction
  y2 = 1 + x1 + 2*x3 + 0.8*x1*x3 + fe * 0.3 + rnorm(n, sd = 0.5)
})

#
## Test numeric × factor interaction ----

fml_int = y1 ~ x1 * x2 | fe

db_int = dbreg(fml_int, data = test_df, vcov = "hc1")
fe_int = feols(fml_int, data = test_df, vcov = "hc1")

# dbreg uses "_x_" for interactions, fixest uses ":"
# Map fixest names to dbreg names for comparison
map_coef_name = function(x) gsub(":", "_x_", x)

# Check estimates match (by mapped name)
for (coef in rownames(fe_int$coeftable)) {
  db_coef = map_coef_name(coef)
  expect_equal(
    db_int$coeftable[db_coef, "estimate"],
    fe_int$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("estimate for", coef)
  )
}

# Check standard errors match
for (coef in rownames(fe_int$coeftable)) {
  db_coef = map_coef_name(coef)
  expect_equal(
    db_int$coeftable[db_coef, "std.error"],
    fe_int$coeftable[coef, "Std. Error"],
    tolerance = 1e-6,
    info = paste("SE for", coef)
  )
}

#
## Test interaction-only term (:) ----

fml_colon = y1 ~ x1 + x2 + x1:x2 | fe

db_colon = dbreg(fml_colon, data = test_df, vcov = "hc1")
fe_colon = feols(fml_colon, data = test_df, vcov = "hc1")

# Should give same results as * formula
expect_equal(
  db_colon$coeftable[, "estimate"],
  db_int$coeftable[, "estimate"],
  tolerance = 1e-6,
  info = "x1:x2 gives same result as x1*x2"
)

#
## Test numeric × numeric interaction ----

fml_num = y2 ~ x1 * x3 | fe

db_num = dbreg(fml_num, data = test_df, vcov = "hc1")
fe_num = feols(fml_num, data = test_df, vcov = "hc1")

for (coef in rownames(fe_num$coeftable)) {
  db_coef = map_coef_name(coef)
  expect_equal(
    db_num$coeftable[db_coef, "estimate"],
    fe_num$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("numeric interaction estimate for", coef)
  )
}

#
## Test sql_only with interactions ----

invisible(capture.output({
  sql_int = dbreg(fml_int, data = test_df, sql_only = TRUE)
}))
expect_true(is.character(sql_int), info = "sql_only returns string")
expect_true(grepl("CASE WHEN", sql_int), info = "SQL contains factor expansion")

#
## Test moments strategy (no FEs) ----

fml_moments = y ~ x1 * x2

db_moments = dbreg(fml_moments, data = test_df, vcov = "hc1", strategy = "moments")
fe_moments = feols(fml_moments, data = test_df, vcov = "hc1")

for (coef in rownames(fe_moments$coeftable)) {
  db_coef = map_coef_name(coef)
  expect_equal(
    db_moments$coeftable[db_coef, "estimate"],
    fe_moments$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("moments estimate for", coef)
  )
  expect_equal(
    db_moments$coeftable[db_coef, "std.error"],
    fe_moments$coeftable[coef, "Std. Error"],
    tolerance = 1e-6,
    info = paste("moments SE for", coef)
  )
}

