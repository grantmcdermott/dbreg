library(dbreg)

set.seed(2026)
n = 600
x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
x4 = rnorm(n)
y = 1.5 * x1 - 0.8 * x2 + 0.5 * x3 * x3 + rnorm(n, sd = 0.5)
w = runif(n, min = 0.2, max = 3)

df = data.frame(
  y = y,
  x1 = x1,
  x2 = x2,
  x3 = x3,
  x4 = x4,
  w = w
)

mod = dbforest(
  y ~ x1 + x2 + x3 + x4,
  data = df,
  ntree = 25,
  mtry = 2,
  sample_frac = 0.8,
  replace = TRUE,
  seed = 99,
  n_bins = 16,
  max_depth = 4,
  min_split = 10,
  min_leaf = 5
)

expect_true(inherits(mod, "dbforest"))
expect_equal(length(mod$trees), 25)
expect_equal(mod$mtry, 2L)
expect_equal(mod$pool_n, ceiling(0.8 * nrow(df)))
expect_equal(mod$ntree, 25L)

pred = predict(mod, df[1:20, ])
expect_equal(length(pred), 20)
expect_true(all(is.finite(pred)))

pred_ind = predict(mod, df[1:10, ], individual = TRUE)
expect_true(is.list(pred_ind))
expect_equal(length(pred_ind$fit), 10)
expect_equal(dim(pred_ind$individual), c(10, 25))
expect_true(all(is.finite(pred_ind$individual)))

print_txt = capture.output(print(mod))
expect_true(any(grepl("DB-native random forest", print_txt)))
expect_true(any(grepl("Trees:", print_txt)))
expect_true(any(grepl("Sampling:", print_txt)))

mod_same_seed = dbforest(
  y ~ x1 + x2 + x3 + x4,
  data = df,
  ntree = 25,
  mtry = 2,
  sample_frac = 0.8,
  replace = TRUE,
  seed = 99,
  n_bins = 16,
  max_depth = 4,
  min_split = 10,
  min_leaf = 5
)
pred_same_seed = predict(mod_same_seed, df[1:40, ])
expect_equal(
  pred_same_seed,
  predict(mod, df[1:40, ]),
  tolerance = 1e-12,
  info = "same seed reproduces the same forest predictions"
)

mod_diff_seed = dbforest(
  y ~ x1 + x2 + x3 + x4,
  data = df,
  ntree = 25,
  mtry = 2,
  sample_frac = 0.8,
  replace = TRUE,
  seed = 100,
  n_bins = 16,
  max_depth = 4,
  min_split = 10,
  min_leaf = 5
)
pred_diff_seed = predict(mod_diff_seed, df[1:40, ])
expect_true(
  mean(abs(pred_diff_seed - predict(mod, df[1:40, ]))) > 1e-6,
  info = "different seed changes forest predictions"
)

mod_subsample = dbforest(
  y ~ x1 + x2 + x3 + x4,
  data = df,
  ntree = 15,
  mtry = 2,
  sample_frac = 0.6,
  replace = FALSE,
  seed = 7,
  n_bins = 12,
  max_depth = 3,
  min_split = 10,
  min_leaf = 4
)
pred_subsample = predict(mod_subsample, df[1:25, ])
expect_equal(length(pred_subsample), 25)
expect_true(all(is.finite(pred_subsample)))

mod_weighted = dbforest(
  y ~ x1 + x2 + x3 + x4,
  data = df,
  weights = "w",
  ntree = 12,
  mtry = 2,
  sample_frac = 0.7,
  replace = TRUE,
  seed = 123,
  n_bins = 12,
  max_depth = 3,
  min_split = 10,
  min_leaf = 4
)
pred_weighted = predict(mod_weighted, df[1:30, ])
expect_true(all(is.finite(pred_weighted)))

expect_error(
  dbforest(y ~ x1 + x2, data = df, ntree = 0),
  "ntree"
)
expect_error(
  dbforest(y ~ x1 + x2, data = df, sample_frac = 0),
  "sample_frac"
)
expect_error(
  dbforest(y ~ x1 + x2, data = df, mtry = 3),
  "mtry"
)
expect_error(
  dbforest(y ~ x1 + x2 | x3, data = df),
  "fixed effects"
)
expect_error(
  dbforest(y ~ x1 * x2, data = df),
  "interaction"
)
