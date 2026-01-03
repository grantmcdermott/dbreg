# Validate dbbinsreg against binsreg package
# Tests that dbbinsreg produces results consistent with binsreg
#
# KNOWN DIFFERENCE: Quantile binning algorithm
# binsreg uses R's quantile(type=2), dbbinsreg uses SQL NTILE().
# This causes small differences (~1-2%) in bin boundaries and thus fitted values.
# See R/dbbinsreg.R for detailed documentation.

if (!requireNamespace("binsreg", quietly = TRUE)) {
  exit_file("binsreg package not installed")
}

library(dbreg)
library(binsreg)

# Test data: continuous x, linear relationship with noise
set.seed(42)
n = 2000
x = runif(n, 0, 10)
y = 2 * x + rnorm(n, sd = 1)
df = data.frame(x = x, y = y)

# Tolerance for fit comparisons (accounts for quantile algorithm difference)
TOL_FIT = 0.05  # 5% relative tolerance
TOL_SE = 0.01   # 1% absolute tolerance for SEs
z = qnorm(0.975)  # For back-calculating SEs from CI width

# Helper to run binsreg without plotting.
# binsreg plots by default and noplot=TRUE skips populating data.plot,
# so we suppress the plot device instead. We also suppress warnings about
# nbins vs IMSE-optimal choice since we're testing with fixed nbins.
run_binsreg = function(...) {
  pdf(NULL)
  on.exit(dev.off())
  suppressWarnings(binsreg(...))
}


#
## Test 1: Canonical binscatter - points = c(0, 0) ----
#

br1 = run_binsreg(y, x, data = df, nbins = 10, dots = c(0, 0))
db1 = dbbinsreg(y ~ x, data = df, nbins = 10, points = c(0, 0), ci = FALSE, plot = FALSE, verbose = FALSE)

# Compare fits (allow for quantile algorithm differences)
br1_fit = br1$data.plot[[1]]$data.dots$fit
db1_fit = db1$points$fit
rel_diff1 = max(abs(br1_fit - db1_fit) / abs(br1_fit))
expect_true(rel_diff1 < TOL_FIT, 
            info = sprintf("points=c(0,0): rel diff %.4f > %.2f", rel_diff1, TOL_FIT))


#
## Test 2: Piecewise linear with continuity - points = c(1, 1) ----
#

br2 = run_binsreg(y, x, data = df, nbins = 10, dots = c(1, 1))
db2 = dbbinsreg(y ~ x, data = df, nbins = 10, points = c(1, 1), ci = FALSE, plot = FALSE, verbose = FALSE)

br2_fit = br2$data.plot[[1]]$data.dots$fit
db2_fit = db2$points$fit
rel_diff2 = max(abs(br2_fit - db2_fit) / abs(br2_fit))
expect_true(rel_diff2 < TOL_FIT,
            info = sprintf("points=c(1,1): rel diff %.4f > %.2f", rel_diff2, TOL_FIT))


#
## Test 3: HC1 standard errors match ----
#

br3 = run_binsreg(y, x, data = df, nbins = 10, dots = c(0, 0), ci = c(0, 0), vce = "HC1")
db3 = dbbinsreg(y ~ x, data = df, nbins = 10, points = c(0, 0), ci = TRUE, vcov = "HC1", plot = FALSE, verbose = FALSE)

# Back-calculate binsreg SE from CI width
br3_ci = br3$data.plot[[1]]$data.ci
br3_se = (br3_ci$ci.r - br3_ci$ci.l) / (2 * z)
db3_se = db3$points$se

# SEs should match closely (small diff from quantile algorithm)
se_diff = max(abs(br3_se - db3_se))
expect_true(se_diff < TOL_SE,
            info = sprintf("HC1 SE diff %.6f > %.2f", se_diff, TOL_SE))

# Verify SEs vary by bin (not constant like iid)
expect_true(diff(range(db3_se)) > 0, info = "HC1 SEs should vary by bin")


#
## Test 4: Heteroskedastic data - SEs should increase with x ----
#

set.seed(42)
y_het = 2 * x + rnorm(n, sd = 0.2 + 0.3 * x)  # Variance increases with x
df_het = data.frame(x = x, y = y_het)

br4 = run_binsreg(y_het, x, data = df_het, nbins = 10, dots = c(0, 0), ci = c(0, 0))
db4 = dbbinsreg(y ~ x, data = df_het, nbins = 10, points = c(0, 0), ci = TRUE, plot = FALSE, verbose = FALSE)

br4_se = (br4$data.plot[[1]]$data.ci$ci.r - br4$data.plot[[1]]$data.ci$ci.l) / (2 * z)
db4_se = db4$points$se

# Both should show increasing SEs
expect_true(br4_se[10] > br4_se[1], info = "binsreg SEs should increase with x")
expect_true(db4_se[10] > db4_se[1], info = "dbbinsreg SEs should increase with x")


#
## Test 5: IID standard errors give constant SE ----
#

db5 = dbbinsreg(y ~ x, data = df, nbins = 10, points = c(0, 0), ci = TRUE, vcov = "iid", plot = FALSE, verbose = FALSE)

# IID should give constant SE across bins
se_range = diff(range(db5$points$se))
expect_true(se_range < 1e-10, info = "IID SEs should be constant across bins")


#
## Test 6: With controls ----
#

set.seed(123)
df$w = rnorm(n)
df$y_ctrl = df$y + 0.5 * df$w

w_mat = as.matrix(df[, "w", drop = FALSE])
br6 = run_binsreg(df$y_ctrl, df$x, w = w_mat, nbins = 10, dots = c(0, 0))
db6 = dbbinsreg(y_ctrl ~ x + w, data = df, nbins = 10, points = c(0, 0), ci = FALSE, plot = FALSE, verbose = FALSE)

br6_fit = br6$data.plot[[1]]$data.dots$fit
db6_fit = db6$points$fit
rel_diff6 = max(abs(br6_fit - db6_fit) / abs(br6_fit))
expect_true(rel_diff6 < TOL_FIT,
            info = sprintf("with controls: rel diff %.4f > %.2f", rel_diff6, TOL_FIT))
