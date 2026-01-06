library(fixest)
library(dbreg)

data("base_did", package = "fixest")
did = transform(base_did, period = factor(period))
did$period = relevel(did$period, ref = "5")

#
## Test collinearity detection (treat absorbed by unit FE) ----

fml_did = y ~ x1 + treat * period | id

fe_did = suppressMessages(feols(fml_did, data = did))
db_did = dbreg(fml_did, data = did, strategy = "demean")

# Check treat was detected as collinear
expect_true(
  "treat" %in% db_did$collin.var,
  info = "treat detected as collinear"
)

# Check key coefficients match fixest (x1 and treat:period interactions)
test_coefs = c("x1", grep("^treat:", rownames(fe_did$coeftable), value = TRUE))

for (coef in test_coefs) {
  expect_equal(
    db_did$coeftable[coef, "estimate"],
    fe_did$coeftable[coef, "Estimate"],
    tolerance = 1e-4,
    info = paste("DiD estimate for", coef)
  )
  expect_equal(
    db_did$coeftable[coef, "std.error"],
    fe_did$coeftable[coef, "Std. Error"],
    tolerance = 1e-4,
    info = paste("DiD SE for", coef)
  )
}

