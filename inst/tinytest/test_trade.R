library(fixest)
library(dbreg)

# dictionary for easy testing across results and methods
dict_db = c("estimate", "std.error", "statistic", "p.values")
names(dict_db) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Load test data
data("trade", package = "fixest")

trade_fml = Euros ~ dist_km | Destination + Origin

# Basic regression with fixed effects (should default to compress)

trade_dbreg = dbreg(
  trade_fml,
  data = trade,
  vcov = 'hc1',
  verbose = FALSE
)

trade_feols = feols(
  trade_fml,
  data = trade,
  vcov = 'hc1',
  lean = TRUE
)

trade_coefs = row.names(trade_feols)

# check results
for (i in seq_along(dict_db)) {
  expect_equal(
    trade_dbreg$coeftable[trade_coefs, dict_db[i]],
    trade_feols$coeftable[trade_coefs, names(dict_db)[i]],
    tolerance = 1e-6
  )
}

# Original vs compressed observation counts
expect_true(trade_dbreg$nobs == 210)


# Query-only mode

trade_query = dbreg(
  trade_fml,
  data = trade,
  verbose = FALSE,
  query_only = TRUE
)
expect_true(is.character(trade_query))
expect_true(grepl("^WITH", trade_query))

# Data-only mode

trade_data = dbreg(
  trade_fml,
  data = trade,
  verbose = FALSE,
  data_only = TRUE
)
expect_true(inherits(trade_data, "data.frame"))
expect_true(nrow(trade_data) == 210)
