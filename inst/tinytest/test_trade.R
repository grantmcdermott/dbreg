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
  vcov = 'hc1'
)

trade_feols = feols(
  trade_fml,
  data = trade,
  vcov = 'hc1',
  lean = TRUE
)

trade_coefs = row.names(trade_feols$coeftable)

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


# SQL query-only mode

invisible(capture.output({
  trade_sql = dbreg(
    trade_fml,
    data = trade,
    verbose = FALSE,
    sql_only = TRUE
  )
}))
expect_true(is.character(trade_sql))
expect_true(grepl("^WITH", trade_sql))

# Data-only mode

trade_data = dbreg(
  trade_fml,
  data = trade,
  data_only = TRUE
)
expect_true(inherits(trade_data, "data.frame"))
expect_true(nrow(trade_data) == 210)


#
## Test explicit strategies ----

# demean strategy (requires balanced panel for 2 FE, so use 1 FE instead)
trade_fml1 = Euros ~ dist_km | Destination
trade_demean1 = dbreg(
  trade_fml1,
  data = trade,
  strategy = "demean",
  vcov = "hc1"
)

trade_feols1 = feols(
  trade_fml1,
  data = trade,
  vcov = 'hc1',
  lean = TRUE
)

trade_coefs1 = row.names(trade_feols1$coeftable)

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
  vcov = "hc1"
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

#
## Test clustered standard errors ----

# Note: we use feols(..., ssc = ssc(K.fixef = "full")) since we don't do the
# small sample correction by default for dbreg; since it's very unlikely to 
# matter/bind for the large datasets that dbreg is intended for

# Test with compress strategy (default for this data)
trade_cluster_compress = dbreg(
  trade_fml,
  data = trade,
  vcov = ~Destination
)

trade_cluster_feols = feols(
  trade_fml,
  data = trade,
  vcov = ~Destination,
  ssc = ssc(K.fixef = "full"),
  lean = TRUE
)

# Check coefficients match
expect_equal(
  trade_cluster_compress$coeftable[trade_coefs, "estimate"],
  trade_cluster_feols$coeftable[trade_coefs, "Estimate"],
  tolerance = 1e-6
)

# Check SEs match (dbreg uses K.fixef="full" equivalent)
expect_equal(
  trade_cluster_compress$coeftable[trade_coefs, "std.error"],
  trade_cluster_feols$coeftable[trade_coefs, "Std. Error"],
  tolerance = 2e-5
)

# Test cluster argument syntax (alternative to vcov formula)
trade_cluster_arg = dbreg(
  trade_fml,
  data = trade,
  cluster = ~Destination
)

expect_equal(
  trade_cluster_arg$coeftable[trade_coefs, "std.error"],
  trade_cluster_compress$coeftable[trade_coefs, "std.error"],
  tolerance = 1e-10
)

# Test ssc = "nested" option (matches fixest default)
trade_cluster_nested = dbreg(
  trade_fml,
  data = trade,
  vcov = ~Destination,
  ssc = "nested"
)

trade_cluster_feols_nested = feols(
  trade_fml,
  data = trade,
  vcov = ~Destination,
  lean = TRUE
)

expect_equal(
  trade_cluster_nested$coeftable[trade_coefs, "std.error"],
  trade_cluster_feols_nested$coeftable[trade_coefs, "Std. Error"],
  tolerance = 2e-5
)

# Test with demean strategy (1 FE)
trade_cluster_demean = dbreg(
  trade_fml1,
  data = trade,
  strategy = "demean",
  vcov = ~Destination
)

trade_cluster_demean_feols = feols(
  trade_fml1,
  data = trade,
  vcov = ~Destination,
  ssc = ssc(K.fixef = "full"),
  lean = TRUE
)

expect_equal(
  trade_cluster_demean$coeftable[trade_coefs, "estimate"],
  trade_cluster_demean_feols$coeftable[trade_coefs, "Estimate"],
  tolerance = 1e-6
)

expect_equal(
  trade_cluster_demean$coeftable[trade_coefs, "std.error"],
  trade_cluster_demean_feols$coeftable[trade_coefs, "Std. Error"],
  tolerance = 2e-5
)

# Test with mundlak strategy
trade_cluster_mundlak = dbreg(
  trade_fml,
  data = trade,
  strategy = "mundlak",
  vcov = ~Destination
)

# Manual Mundlak with feols and clustered SEs
trade_cluster_mundlak_manual = feols(
  Euros ~ dist_km + dist_km_mean_dest + dist_km_mean_orig,
  data = trade,
  vcov = ~Destination,
  lean = TRUE
)

expect_equal(
  trade_cluster_mundlak$coeftable[trade_coefs, "estimate"],
  trade_cluster_mundlak_manual$coeftable[trade_coefs, "Estimate"],
  tolerance = 1e-6
)

expect_equal(
  trade_cluster_mundlak$coeftable[trade_coefs, "std.error"],
  trade_cluster_mundlak_manual$coeftable[trade_coefs, "Std. Error"],
  tolerance = 2e-5
)

# Test with moments strategy (no FE)
#
## Test alternating projections (3+ FEs) ----

# AP needs a slightly looser convergence tolerance for larger datasets due to
# floating-point noise from temp-table round-trips
old_tol = getOption("dbreg.ap_tol")
options(dbreg.ap_tol = 1e-7)

# 3 FE (Origin + Destination + Product): auto should select demean via AP
trade_3fe = dbreg(
  Euros ~ dist_km | Origin + Destination + Product,
  data = trade,
  vcov = "iid"
)
trade_3fe_feols = feols(
  Euros ~ dist_km | Origin + Destination + Product,
  data = trade,
  vcov = "iid",
  fixef.rm = "none",
  lean = TRUE
)

expect_true(trade_3fe$strategy == "demean", info = "3 FE auto-selects demean (AP)")
expect_equal(
  trade_3fe$coeftable["dist_km", "estimate"],
  trade_3fe_feols$coeftable["dist_km", "Estimate"],
  tolerance = 1e-6
)
expect_equal(
  trade_3fe$coeftable["dist_km", "std.error"],
  trade_3fe_feols$coeftable["dist_km", "Std. Error"],
  tolerance = 1e-6
)

# 4 FE (Origin + Destination + Product + Year): auto should select demean via AP
trade_4fe = dbreg(
  Euros ~ dist_km | Origin + Destination + Product + Year,
  data = trade,
  vcov = "iid"
)
trade_4fe_feols = feols(
  Euros ~ dist_km | Origin + Destination + Product + Year,
  data = trade,
  vcov = "iid",
  fixef.rm = "none",
  lean = TRUE
)

expect_true(trade_4fe$strategy == "demean", info = "4 FE auto-selects demean (AP)")
expect_equal(
  trade_4fe$coeftable["dist_km", "estimate"],
  trade_4fe_feols$coeftable["dist_km", "Estimate"],
  tolerance = 1e-6
)
expect_equal(
  trade_4fe$coeftable["dist_km", "std.error"],
  trade_4fe_feols$coeftable["dist_km", "Std. Error"],
  tolerance = 1e-6
)

options(dbreg.ap_tol = old_tol)

#
## Test clustered standard errors ----

trade_fml_nofe = Euros ~ dist_km
trade_cluster_moments = dbreg(
  trade_fml_nofe,
  data = trade,
  strategy = "moments",
  vcov = ~Destination
)

trade_cluster_moments_feols = feols(
  trade_fml_nofe,
  data = trade,
  vcov = ~Destination,
  lean = TRUE
)

expect_equal(
  trade_cluster_moments$coeftable["dist_km", "estimate"],
  trade_cluster_moments_feols$coeftable["dist_km", "Estimate"],
  tolerance = 1e-6
)

expect_equal(
  trade_cluster_moments$coeftable["dist_km", "std.error"],
  trade_cluster_moments_feols$coeftable["dist_km", "Std. Error"],
  tolerance = 2e-5
)

