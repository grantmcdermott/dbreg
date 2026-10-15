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
## Test interaction-only without main effect (all factor levels retained) ----

fml_colon_only = y ~ x2:x1
fe_colon_only = feols(fml_colon_only, data = test_df)
db_colon_only = dbreg(fml_colon_only, data = test_df, strategy = "moments")

# Should have all factor levels (a, b, c) since x1 is not a main effect
expect_equal(
  nrow(db_colon_only$coeftable),
  nrow(fe_colon_only$coeftable),
  info = "interaction-only retains all factor levels"
)
for (coef in rownames(fe_colon_only$coeftable)) {
  expect_equal(
    db_colon_only$coeftable[coef, "estimate"],
    fe_colon_only$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("interaction-only estimate for", coef)
  )
}

#
## Test compress strategy with interaction + main effect (x:z + z) ----

fml_compress_int = y1 ~ x2:x1 + x2 | fe
fe_compress_int = feols(fml_compress_int, data = test_df)
db_compress_int = dbreg(fml_compress_int, data = test_df, strategy = "compress")

for (coef in rownames(fe_compress_int$coeftable)) {
  expect_equal(
    db_compress_int$coeftable[coef, "estimate"],
    fe_compress_int$coeftable[coef, "Estimate"],
    tolerance = 1e-6,
    info = paste("compress interaction+main estimate for", coef)
  )
}

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
## Test mundlak strategy vs FE (#77) ----

# With one FE, Mundlak equals FE when every regressor column (incl. factor
# dummies and interactions) gets a group mean
for (fml_mundlak in list(y2 ~ x1 * x3 | fe, y1 ~ x1 * x2 | fe, y1 ~ x1 + x2 | fe)) {
  fe_mundlak = coef(feols(fml_mundlak, data = test_df))
  expect_equal(
    coef(dbreg(fml_mundlak, data = test_df, strategy = "mundlak"))[names(fe_mundlak)],
    fe_mundlak,
    tolerance = 1e-6,
    info = paste("one-way mundlak matches FE:", deparse(fml_mundlak))
  )
}

# Two-way Mundlak equals TWFE in balanced panels (Wooldridge 2021)
set.seed(77)
dat_tw = expand.grid(id = 1:30, t = 1:6)
dat_tw = transform(dat_tw, x = rnorm(180) + id / 10, z = rnorm(180), g = sample(c("a", "b", "c"), 180, TRUE))
dat_tw$y = dat_tw$x * dat_tw$z + (dat_tw$g == "c") + dat_tw$id / 5 + dat_tw$t + rnorm(180)
fe_tw = coef(feols(y ~ x * z + g | id + t, data = dat_tw))
expect_equal(
  coef(dbreg(y ~ x * z + g | id + t, data = dat_tw, strategy = "mundlak"))[names(fe_tw)],
  fe_tw,
  tolerance = 1e-8,
  info = "two-way mundlak matches TWFE in a balanced panel"
)

# A time-invariant regressor keeps its coefficient and drops its (collinear)
# group mean, as in CRE
test_df_tinv = transform(test_df, tinv = fe %% 3, x1_bar = ave(x1, fe))
db_tinv = dbreg(y1 ~ x1 + tinv | fe, data = test_df_tinv, strategy = "mundlak")
cre_tinv = coef(lm(y1 ~ x1 + tinv + x1_bar, data = test_df_tinv))
expect_equal(coef(db_tinv)[c("x1", "tinv")], cre_tinv[c("x1", "tinv")], tolerance = 1e-6, info = "mundlak time-invariant regressor matches CRE")
expect_equal(db_tinv[["collin.var"]], "tinv_bar_fe", info = "mundlak drops the time-invariant group mean")

#
## Test sql_only with interactions ----

invisible(capture.output({
  sql_int = dbreg(fml_int, data = test_df, sql_only = TRUE)
}))
expect_true(is.character(sql_int), info = "sql_only returns string")
expect_true(grepl("CASE WHEN", sql_int), info = "SQL contains factor expansion")


#
## No-intercept models (#68) ----

set.seed(68)
dat68 = data.frame(x = rnorm(200), x2 = sample(1:5, 200, TRUE), g = sample(c("a", "b", "c"), 200, TRUE))
dat68$y = 1 + 2 * dat68$x + (dat68$g == "b") + rnorm(200)
db68 = dbreg(y ~ 0 + x, data = dat68, strategy = "moments")
expect_equal(coef(db68, fe = TRUE), coef(feols(y ~ 0 + x, data = dat68)), tolerance = 1e-10, info = "moments honours no intercept")
fx68c = feols(y ~ 0 + x2 + g, data = dat68)
expect_equal(
  coef(dbreg(y ~ 0 + x2 + g, data = dat68, strategy = "compress"), fe = TRUE)[names(coef(fx68c))],
  coef(fx68c),
  tolerance = 1e-10,
  info = "compress no-intercept keeps all levels of the first factor"
)
expect_equal(
  unname(coef(dbreg(y ~ 0 + g * x, data = dat68, strategy = "moments"), fe = TRUE)[names(coef(lm(y ~ 0 + g * x, data = dat68)))]),
  unname(coef(lm(y ~ 0 + g * x, data = dat68))),
  tolerance = 1e-10,
  info = "moments no-intercept with a factor interaction keeps all levels of the first factor"
)
expect_equal(
  as.numeric(predict(db68, newdata = dat68)),
  as.numeric(predict(lm(y ~ 0 + x, data = dat68))),
  tolerance = 1e-10,
  info = "no-intercept predictions match lm"
)

#
## predict() with interactions, incl. mundlak (#67) ----

set.seed(3)
dat67 = data.frame(fe = factor(sample(1:8, 80, TRUE)), x1 = rnorm(80), x3 = rnorm(80))
dat67$x1_bar_fe = ave(dat67$x1, dat67$fe, FUN = mean)
dat67$x3_bar_fe = ave(dat67$x3, dat67$fe, FUN = mean)
dat67$y = 1 + 2 * dat67$x1 - 0.5 * dat67$x3 + 1.2 * dat67$x1 * dat67$x3 +
  0.8 * dat67$x1_bar_fe - 0.3 * dat67$x3_bar_fe + rnorm(80, sd = 0.1)
mm67 = model.matrix(~ x1 * x3, dat67)[, -1]
bar67 = apply(mm67, 2, ave, dat67$fe)
expect_equal(
  as.numeric(predict(dbreg(y ~ x1 * x3 | fe, data = dat67, strategy = "mundlak"), newdata = dat67)),
  as.numeric(fitted(lm(dat67$y ~ mm67 + bar67))),
  tolerance = 1e-6,
  info = "mundlak predict with interactions matches lm (#67)"
)
expect_equal(
  as.numeric(predict(dbreg(y ~ x1 * x3 | fe, data = dat67, strategy = "demean"), newdata = dat67)),
  as.numeric(predict(feols(y ~ x1 * x3 | fe, data = dat67), newdata = dat67)),
  tolerance = 1e-6,
  info = "demean predict with interactions matches fixest"
)

#
## Plain factor regressors (no interactions) ----

set.seed(70)
dat_pf = data.frame(fe = factor(sample(1:10, 300, TRUE)), x = rnorm(300), g = sample(c("a", "b", "c"), 300, TRUE))
dat_pf$y = dat_pf$x + (dat_pf$g == "b") + as.integer(dat_pf$fe) / 5 + rnorm(300)
for (s_pf in c("moments", "demean")) {
  f_pf = if (s_pf == "moments") y ~ x + g else y ~ x + g | fe
  expect_equal(
    coef(dbreg(f_pf, data = dat_pf, strategy = s_pf)),
    coef(feols(f_pf, data = dat_pf)),
    tolerance = 1e-6,
    info = paste(s_pf, "handles a plain factor regressor")
  )
}

#
## Factor levels come from all rows, not a sample ----

dat_lv = data.frame(x = rnorm(100001), g = c(rep("a", 100000), "z"))
dat_lv$y = dat_lv$x + rnorm(100001)
expect_true(
  "x:gz" %in% names(coef(dbreg(y ~ x:g, data = dat_lv, strategy = "moments"))),
  info = "a factor level appearing only after row 100,000 is kept"
)
