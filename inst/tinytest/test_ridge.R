library(Matrix)
library(dbreg)

# -----------------------------------------------------------------------------
# Ridge: moments strategy (no FE)
# -----------------------------------------------------------------------------

set.seed(101)
n = 200
df = data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n)
)

lambda = 1.5
mod_moments = dbreg(y ~ x1 + x2, data = df, strategy = "moments", ridge = lambda)

# Closed-form ridge (intercept not penalized)
X = model.matrix(~ x1 + x2, data = df)
y = df$y
XtX = crossprod(X)
Xty = crossprod(X, y)
penalize_idx = 2:ncol(X)
XtX_pen = XtX
diag_vals = diag(XtX_pen)
diag_vals[penalize_idx] = diag_vals[penalize_idx] + lambda
diag(XtX_pen) = diag_vals
beta_expected = solve(XtX_pen, Xty)
beta_expected = as.numeric(beta_expected)
names(beta_expected) = colnames(X)

coefs_moments = mod_moments$coeftable[names(beta_expected), "estimate"]
expect_equal(
  as.numeric(coefs_moments),
  unname(beta_expected),
  tolerance = 1e-6,
  info = "moments ridge coefficients match closed-form solution"
)
expect_true(
  all(is.na(mod_moments$coeftable[, "std.error"])),
  info = "ridge SEs are NA"
)
expect_equal(
  attr(mod_moments$vcov, "type"),
  "ridge",
  info = "vcov type is ridge"
)

# -----------------------------------------------------------------------------
# Ridge: compress strategy with FE (FE dummies unpenalized)
# -----------------------------------------------------------------------------

set.seed(102)
n = 300
df2 = data.frame(
  y = rnorm(n),
  x1 = sample(c("a", "b", "c"), n, replace = TRUE),
  x2 = sample(c("low", "high"), n, replace = TRUE),
  fe = sample(1:6, n, replace = TRUE)
)

lambda2 = 2.0
mod_compress = dbreg(y ~ x1 + x2 | fe, data = df2, strategy = "compress", ridge = lambda2)

# Use compressed data to replicate weighted ridge
compressed = dbreg(
  y ~ x1 + x2 | fe,
  data = df2,
  strategy = "compress",
  data_only = TRUE
)

Xc = sparse.model.matrix(reformulate(c("x1", "x2", "fe")), compressed)
Yc = compressed[, "mean_Y"]
wts = compressed[["wts"]]
Xw = Xc * wts
Yw = Yc * wts
XtX = as.matrix(crossprod(Xw))
XtY = as.matrix(crossprod(Xw, Yw))

# Penalize only non-intercept, non-FE columns
assign = attr(Xc, "assign")
terms_obj = attr(Xc, "terms")
term_labels = if (!is.null(terms_obj)) attr(terms_obj, "term.labels") else character(0)
fe_term_idx = match("fe", term_labels)
penalize = rep(TRUE, ncol(Xc))
penalize[assign == 0] = FALSE
penalize[assign %in% fe_term_idx] = FALSE

XtX_pen = XtX
diag_vals = diag(XtX_pen)
diag_vals[penalize] = diag_vals[penalize] + lambda2
diag(XtX_pen) = diag_vals
beta_expected2 = solve(XtX_pen, XtY)
beta_expected2 = as.numeric(beta_expected2)
names(beta_expected2) = colnames(Xc)

coefs_compress = mod_compress$coeftable[names(beta_expected2), "estimate"]
expect_equal(
  as.numeric(coefs_compress),
  unname(beta_expected2),
  tolerance = 1e-6,
  info = "compress ridge coefficients match weighted closed-form solution"
)
