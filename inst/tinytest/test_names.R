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
