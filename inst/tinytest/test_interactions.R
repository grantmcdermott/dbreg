library(fixest)
library(dbreg)

# Create a test dataset with a factor variable
set.seed(123)
test_df = data.frame(
  y = rnorm(100),
  x1 = rnorm(100),
  x2 = sample(c("a", "b", "c"), 100, replace = TRUE),
  fe = sample(1:5, 100, replace = TRUE)
)

#
## Test numeric × factor interaction ----

fml_int = y ~ x1 * x2 | fe

db_int = dbreg(fml_int, data = test_df, vcov = "hc1")
fe_int = feols(fml_int, data = test_df, vcov = "hc1")

# dbreg uses "_" for interactions, fixest uses ":"
# Map fixest names to dbreg names for comparison
map_coef_name = function(x) gsub(":", "_", x)

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

fml_colon = y ~ x1 + x2 + x1:x2 | fe

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

test_df$x3 = rnorm(100)
fml_num = y ~ x1 * x3 | fe

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

