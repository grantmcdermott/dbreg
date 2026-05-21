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

# Test 3: Character cluster input must name exactly one variable
expect_error(
  dbreg(Temp ~ Wind, data = airquality, cluster = c("Month", "Day")),
  "Only single-variable clustering is currently supported"
)

# Test 4: In-memory data requires DuckDB when conn is supplied
fake_conn = structure(list(), class = "not_a_connection")
expect_error(
  dbreg(Temp ~ Wind, conn = fake_conn, data = airquality),
  "In-memory data frames are only supported with DuckDB connections"
)
