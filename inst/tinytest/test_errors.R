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

# Test 5: Transformed terms are rejected rather than silently dropped
expect_error(dbreg(log(Temp) ~ Wind, data = airquality), "Transformed or function terms .* outcome")
expect_error(dbreg(Temp ~ log(Wind), data = airquality), "Transformed or function terms .* regressors")
expect_error(dbreg(Temp ~ Wind + I(Wind^2), data = airquality), "regressors: I\\(Wind\\^2\\)")
expect_error(dbreg(Temp ~ Wind | factor(Month), data = airquality), "fixed effects")

# Test 6: predict() refuses demean models with more than two FE
aq = na.omit(airquality)
aq$wk = rep(1:5, length.out = nrow(aq))
expect_error(
  predict(dbreg(Temp ~ Wind | Month + Day + wk, data = aq, strategy = "demean"), newdata = aq),
  "more than two fixed effects"
)

# Test 7: Misspelled weight column on a database table gives an informative error
con_w = DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con_w, "aq", na.omit(airquality))
expect_error(
  dbreg(Temp ~ Wind, conn = con_w, table = "aq", weights = "Wnd"),
  "Could not validate weight column 'Wnd'"
)

# Test 8: In-memory data registered on a user connection is released afterwards
invisible(dbreg(Temp ~ Wind, conn = con_w, data = na.omit(airquality)))
expect_equal(DBI::dbListTables(con_w), "aq", info = "registered data released after dbreg()")
expect_error(dbreg(Temp ~ Wind, conn = con_w, data = na.omit(airquality), compress_ratio = 2))
expect_equal(DBI::dbListTables(con_w), "aq", info = "registered data released after a validation error")
DBI::dbDisconnect(con_w, shutdown = TRUE)
