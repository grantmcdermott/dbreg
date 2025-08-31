library(dbreg)

# Load test data
data("airquality")

# Test 1: Error handling - no regressors
expect_error(dbreg(Temp ~ 1, data = airquality), "No regressors on RHS")

# Test 2: Error handling - multiple outcome variables
expect_error(
  dbreg(cbind(Temp, Wind) ~ Solar.R, data = airquality),
  "Exactly one outcome variable required"
)