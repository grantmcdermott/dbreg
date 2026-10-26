library(dbreg)
library(fixest)

# Variable names and factor levels that aren't SQL-safe identifiers (#58)

set.seed(58)
ir = iris
ir$w.t = runif(nrow(ir))
ir$cl.id = rep(1:15, 10)
ir$order = rnorm(nrow(ir)) # reserved SQL word
ir$`my grp` = sample(c("control", "treated group"), nrow(ir), replace = TRUE)

# Dotted names (the #58 example)
db1 = dbreg(Sepal.Length ~ Petal.Length | Species, data = ir)
fx1 = feols(Sepal.Length ~ Petal.Length | Species, data = ir)
expect_equal(coef(db1), coef(fx1), tolerance = 1e-6, info = "dotted names: coefficients and names match fixest")
expect_equal(db1$yvar, "Sepal.Length", info = "dotted names: outcome name restored")

# Dotted weights and cluster names, plus a reserved word (moments)
db2 = dbreg(Sepal.Length ~ Petal.Length + order, data = ir, weights = "w.t", vcov = ~cl.id, strategy = "moments")
fx2 = feols(Sepal.Length ~ Petal.Length + order, data = ir, weights = ~w.t, vcov = ~cl.id)
expect_equal(coef(db2), coef(fx2), tolerance = 1e-10, info = "weights/cluster/reserved names: coefficients")
expect_equal(as.numeric(db2$coeftable[, "std.error"]), as.numeric(se(fx2)), tolerance = 1e-6, info = "weights/cluster/reserved names: clustered SEs")

# Non-syntactic variable name with a factor level containing a space (compress)
db3 = dbreg(Sepal.Length ~ Petal.Length + `my grp`, data = ir, strategy = "compress")
lm3 = lm(Sepal.Length ~ Petal.Length + `my grp`, data = ir)
expect_equal(coef(db3, fe = TRUE), coef(lm3), tolerance = 1e-10, info = "spaced names/levels: coefficients and lm-style names")
expect_equal(as.numeric(predict(db3, newdata = ir)), as.numeric(predict(lm3)), tolerance = 1e-10, info = "spaced names/levels: predict")
expect_equal(
  names(dbreg(Sepal.Length ~ Petal.Length | Species, data = ir, strategy = "compress", data_only = TRUE))[1:2],
  c("Petal.Length", "Species"),
  info = "data_only restores column names"
)

# dbivreg with dotted names, a factor (spaced levels) and FE
# (syntactic factor name here: fixest's IV first stage drops backticks)
ir$grp = ir$`my grp`
fx_iv = feols(Sepal.Length ~ grp | Species | Petal.Length ~ Sepal.Width, data = ir)
fx_iv_coef = setNames(coef(fx_iv), sub("^fit_", "", names(coef(fx_iv))))
expect_equal(
  coef(dbivreg(Sepal.Length ~ grp | Species | Petal.Length ~ Sepal.Width, data = ir))[names(fx_iv_coef)],
  fx_iv_coef,
  tolerance = 1e-6,
  info = "dbivreg: dotted names with a spaced-level factor and FE"
)

# dbbinsreg with dotted names
bins_dot = dbbinsreg(Sepal.Length ~ Petal.Length | Species, data = ir, nbins = 5, plot = FALSE)
bins_ref = dbbinsreg(y ~ x | f, data = data.frame(y = ir$Sepal.Length, x = ir$Petal.Length, f = ir$Species), nbins = 5, plot = FALSE)
expect_equal(bins_dot$points$fit, bins_ref$points$fit, tolerance = 1e-10, info = "dbbinsreg: dotted names give the same fit")
expect_equal(bins_dot$opt$x_var, "Petal.Length", info = "dbbinsreg: display names restored")

# dbbinsreg with separate points/line fits restores names in both
bins_sep = dbbinsreg(Sepal.Length ~ Petal.Length + Sepal.Width, data = ir, nbins = 5, points = c(0, 0), line = c(1, 1), plot = FALSE)
expect_true(
  "Sepal.Width" %in% rownames(bins_sep$model$points$coeftable) && "Sepal.Width" %in% rownames(bins_sep$model$line$coeftable),
  info = "dbbinsreg: names restored in separate points/line fits"
)

# Identifier rules on case-folding backends (Postgres, Athena/Trino)
expect_equal(
  dbreg:::is_safe_identifier(c("temp", "Temp", "sepal_length"), fold_case = TRUE),
  c(TRUE, FALSE, TRUE),
  info = "fold_case: only lower-case names are safe"
)
expect_equal(
  dbreg:::safe_identifiers(c("Temp", "temp.x", "Sepal.Length"), fold_case = TRUE),
  c("temp", "temp_x", "sepal_length"),
  info = "fold_case: generated aliases are lower case"
)
expect_equal(
  dbreg:::safe_identifiers(c("Sepal_Length", "sepal.length")),
  c("Sepal_Length", "sepal_length_1"),
  info = "aliases are de-duplicated case-insensitively"
)

# End to end with case folding (forced here, since CI only has DuckDB)
folds_orig = dbreg:::folds_identifier_case
assignInNamespace("folds_identifier_case", function(conn) TRUE, "dbreg")
db_fold = dbreg(Sepal.Length ~ Petal.Length + Petal.Width:`my grp` | Species, data = ir, strategy = "demean")
assignInNamespace("folds_identifier_case", folds_orig, "dbreg")
fx_fold = feols(Sepal.Length ~ Petal.Length + Petal.Width:`my grp` | Species, data = ir)
expect_equal(coef(db_fold)[names(coef(fx_fold))], coef(fx_fold), tolerance = 1e-6, info = "fold_case end to end: coefficients and names")

#
## Model variables may use the internal `dbreg_` prefix ----

dat_res = transform(fixest::base_did, dbreg_cell = treat, dbreg_cl = id)
fx_res = fixest::feols(y ~ dbreg_cell + dbreg_cell:post | period, data = dat_res, vcov = ~dbreg_cl)
db_res = dbreg(y ~ dbreg_cell + dbreg_cell:post | period, data = dat_res, vcov = ~dbreg_cl, ssc = "nested", strategy = "compress")
expect_equal(coef(db_res)[names(coef(fx_res))], coef(fx_res), tolerance = 1e-10, info = "reserved-prefix names: compress coefficients")
expect_equal(
  as.numeric(db_res$coeftable[names(coef(fx_res)), "std.error"]),
  as.numeric(fixest::se(fx_res)),
  tolerance = 1e-10,
  info = "reserved-prefix names: compress clustered SEs"
)
dat_res$dbreg_cell2 = as.integer(dat_res$x1 + sin(seq_len(nrow(dat_res))) > 0)
db_iv_res = dbivreg(y ~ post | dbreg_cell ~ dbreg_cell2, data = dat_res, vcov = ~dbreg_cl, strategy = "compress")
se_iv_res = fixest::se(fixest::feols(y ~ post | dbreg_cell ~ dbreg_cell2, data = dat_res, vcov = ~dbreg_cl))
names(se_iv_res) = sub("^fit_", "", names(se_iv_res))
expect_equal(
  as.numeric(db_iv_res$coeftable[names(se_iv_res), "std.error"]),
  as.numeric(se_iv_res),
  tolerance = 1e-10,
  info = "reserved-prefix names: compressed IV clustered SEs"
)
