library(fixest)
library(dbreg)

# dictionary for easy testing across results and methods
dict_db = c("estimate", "std.error", "statistic", "p.values")
names(dict_db) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Load test data
data("trade", package = "fixest")

trade_fml = Euros ~ dist_km | Destination + Origin

#
## Test auto strategy -----

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

invisible(capture.output({
  trade_query = dbreg(
    trade_fml,
    data = trade,
    verbose = FALSE,
    query_only = TRUE
  )
}))
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


#
## Test explicit strategies ----

# demean strategy (requires balanced panel for 2 FE, so use 1 FE instead)
trade_fml1 = Euros ~ dist_km | Destination
trade_demean1 = dbreg(
  trade_fml,
  data = trade,
  strategy = "demean",
  vcov = "hc1",
  verbose = FALSE
)

trade_feols1 = feols(
  trade_fml1,
  data = trade,
  vcov = 'hc1',
  lean = TRUE
)

trade_coefs1 = row.names(trade_feols1)

for (i in seq_along(dict_db)) {
  expect_equal(
    trade_demean1$coeftable[trade_coefs1, dict_db[i]],
    trade_feols1$coeftable[trade_coefs1, names(dict_db)[i]],
    tolerance = 1e-6
  )
}

# mundlak strategy (comparison with manual feols implementation)
# Create group means for manual Mundlak
trade$dist_km_mean_dest = ave(trade$dist_km, trade$Destination, FUN = mean)
trade$dist_km_mean_orig = ave(trade$dist_km, trade$Origin, FUN = mean)

trade_mundlak = dbreg(
  trade_fml,
  data = trade,
  strategy = "mundlak",
  vcov = "hc1",
  verbose = FALSE
)

# Manual Mundlak with feols (no FE, just group means as covariates)
trade_mundlak_manual = feols(
  Euros ~ dist_km + dist_km_mean_dest + dist_km_mean_orig,
  data = trade,
  vcov = "hc1",
  lean = TRUE
)

for (i in seq_along(dict_db)) {
  expect_equal(
    trade_mundlak$coeftable[trade_coefs, dict_db[i]],
    trade_mundlak_manual$coeftable[trade_coefs, names(dict_db)[i]],
    tolerance = 1e-6
  )
}

