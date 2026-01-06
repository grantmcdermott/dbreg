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
  # y: no FE effect (for moments strategy)
  y = 1 + 2*x1 + 3*(x2 == "b") + 4*(x2 == "c") +
      0.5*x1*(x2 == "b") - 1*x1*(x2 == "c") + rnorm(n, sd = 0.5)
  # y1: includes FE effect (numeric × factor interaction)
  y1 = y + fe * 0.5
  # y2: numeric × numeric interaction
  y2 = 1 + x1 + 2*x3 + 0.8*x1*x3 + fe * 0.3 + rnorm(n, sd = 0.5)
})

#
## Test numeric × factor interaction (with FE) ----

fml_int = y1 ~ x1 * x2 | fe

fe_int = feols(fml_int, data = test_df, vcov = "hc1")
db_compress = dbreg(fml_int, data = test_df, vcov = "hc1", strategy = "compress")
db_demean = dbreg(fml_int, data = test_df, vcov = "hc1", strategy = "demean")

for (coef in rownames(fe_int$coeftable)) {
  # compress
  expect_equal(
    db_compress$coeftable[coef, "estimate"],
    fe_int$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("compress estimate for", coef)
  )
  expect_equal(
    db_compress$coeftable[coef, "std.error"],
    fe_int$coeftable[coef, "Std. Error"],
    tolerance = 1e-6,
    info = paste("compress SE for", coef)
  )
  # demean
  expect_equal(
    db_demean$coeftable[coef, "estimate"],
    fe_int$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("demean estimate for", coef)
  )
  expect_equal(
    db_demean$coeftable[coef, "std.error"],
    fe_int$coeftable[coef, "Std. Error"],
    tolerance = 1e-6,
    info = paste("demean SE for", coef)
  )
}

#
## Test interaction-only term (:) ----

fml_colon = y1 ~ x1 + x2 + x1:x2 | fe
db_colon = dbreg(fml_colon, data = test_df, vcov = "hc1", strategy = "compress")

expect_equal(
  db_colon$coeftable[, "estimate"],
  db_compress$coeftable[, "estimate"],
  tolerance = 1e-6,
  info = "x1:x2 gives same result as x1*x2"
)

#
## Test numeric × numeric interaction ----

fml_num = y2 ~ x1 * x3 | fe

fe_num = feols(fml_num, data = test_df, vcov = "hc1")
db_num_compress = dbreg(fml_num, data = test_df, vcov = "hc1", strategy = "compress")
db_num_demean = dbreg(fml_num, data = test_df, vcov = "hc1", strategy = "demean")

for (coef in rownames(fe_num$coeftable)) {
  expect_equal(
    db_num_compress$coeftable[coef, "estimate"],
    fe_num$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("numeric compress estimate for", coef)
  )
  expect_equal(
    db_num_demean$coeftable[coef, "estimate"],
    fe_num$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("numeric demean estimate for", coef)
  )
}

#
## Test moments strategy (no FEs) ----

fml_moments = y ~ x1 * x2

fe_moments = feols(fml_moments, data = test_df, vcov = "hc1")
db_moments = dbreg(fml_moments, data = test_df, vcov = "hc1", strategy = "moments")

for (coef in rownames(fe_moments$coeftable)) {
  expect_equal(
    db_moments$coeftable[coef, "estimate"],
    fe_moments$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("moments estimate for", coef)
  )
  expect_equal(
    db_moments$coeftable[coef, "std.error"],
    fe_moments$coeftable[coef, "Std. Error"],
    tolerance = 1e-6,
    info = paste("moments SE for", coef)
  )
}

#
## Test mundlak strategy (numeric interaction) ----

fml_mundlak = y2 ~ x1 * x3 | fe

db_mundlak = dbreg(fml_mundlak, data = test_df, strategy = "mundlak")

# Compare against manual mundlak via feols
test_df_mundlak = transform(test_df, x1_bar = ave(x1, fe), x3_bar = ave(x3, fe))
fe_mundlak = feols(y2 ~ x1 * x3 + x1_bar + x3_bar, data = test_df_mundlak)

for (coef in c("x1", "x3", "x1:x3")) {
  expect_equal(
    db_mundlak$coeftable[coef, "estimate"],
    fe_mundlak$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("mundlak estimate for", coef)
  )
}

#
## Test sql_only with interactions ----

invisible(capture.output({
  sql_int = dbreg(fml_int, data = test_df, sql_only = TRUE)
}))
expect_true(is.character(sql_int), info = "sql_only returns string")
expect_true(grepl("CASE WHEN", sql_int), info = "SQL contains factor expansion")

