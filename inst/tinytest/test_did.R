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


#
## Test two-way FE (treat:period with id + period FE) ----

fml_twfe = y ~ x1 + treat:period | id + period

db_twfe = dbreg(fml_twfe, data = did, strategy = "demean")

# Check key coefficients match fixest (same as above, period FE absorbs period main effects)
test_coefs_twfe = c("x1", grep("^treat:", rownames(fe_did$coeftable), value = TRUE))

for (coef in test_coefs_twfe) {
  expect_equal(
    db_twfe$coeftable[coef, "estimate"],
    fe_did$coeftable[coef, "Estimate"],
    tolerance = 1e-4,
    info = paste("TWFE estimate for", coef)
  )
}


#
## fixest-style i() terms (#56) ----

base_did$g = rep_len(c("a", "it's b", "c d"), nrow(base_did))
base_did$lg = base_did$treat == 1
base_did$fd = c(0.1, 0.2, 1/3, 0.1 + 0.2)[base_did$period %% 4 + 1]
i_cases = list(
  list(fml = y ~ x1 + i(period, treat, ref = 5) | id + period, strategies = c("compress", "demean", "mundlak")),
  list(fml = y ~ x1 + i(period, treat, ref = 5, keep = 3:8) | id, strategies = c("compress", "demean")),
  list(fml = y ~ x1 + i(period), strategies = c("compress", "moments")),
  list(fml = y ~ 0 + i(period), strategies = c("compress", "moments")),
  list(fml = y ~ i(g, x1, ref = "a") + i(period, lg, ref = 1), strategies = c("compress", "moments")),
  # keep without ref keeps the first level; unmatched ref values are ignored
  list(fml = y ~ x1 + i(period, keep = 1:3), strategies = c("compress", "moments")),
  list(fml = y ~ x1 + i(period, keep = 1:3) | id, strategies = c("compress", "demean")),
  list(fml = y ~ x1 + i(period, ref = c(5, 99)), strategies = "moments"),
  # double-valued levels (incl. ones as.character() rounds) match exactly
  list(fml = y ~ x1 + i(fd), strategies = c("compress", "moments"))
)
for (case in i_cases) {
  fx_i = coef(feols(case[["fml"]], data = base_did, notes = FALSE))
  for (s in case[["strategies"]]) {
    db_i = coef(dbreg(case[["fml"]], data = base_did, strategy = s))
    expect_equal(sort(names(db_i)), sort(names(fx_i)), info = paste(s, "i() coefficient names:", deparse(case[["fml"]])))
    expect_equal(db_i[names(fx_i)], fx_i, tolerance = 1e-10, info = paste(s, "i() estimates:", deparse(case[["fml"]])))
  }
}

# Clustered SEs (incl. compress, whose cluster cells use the expanded columns)
fml_i = y ~ x1 + i(period, treat, ref = 5) | id + period
fx_i = feols(fml_i, data = base_did, vcov = ~id)
for (s in c("compress", "demean")) {
  db_i = dbreg(fml_i, data = base_did, strategy = s, vcov = ~id, ssc = "nested")
  expect_equal(as.numeric(db_i$coeftable[names(se(fx_i)), "std.error"]), as.numeric(se(fx_i)), tolerance = 1e-10, info = paste(s, "i() clustered SEs"))
  expect_equal(
    as.numeric(predict(db_i, newdata = base_did)),
    as.numeric(predict(fx_i)),
    tolerance = 1e-10,
    info = paste(s, "i() predictions")
  )
}
fx_int = feols(y ~ x1 * g | id, data = base_did, vcov = ~id)
expect_equal(
  as.numeric(dbreg(y ~ x1 * g | id, data = base_did, strategy = "compress", vcov = ~id, ssc = "nested")$coeftable[names(se(fx_int)), "std.error"]),
  as.numeric(se(fx_int)),
  tolerance = 1e-10,
  info = "compress clustered SEs work with plain interactions"
)

# IV: i() in the exogenous regressors, instruments and endogenous regressors
base_did$z = base_did$x1 + sin(seq_len(nrow(base_did)))
base_did$xe = base_did$z + cos(seq_len(nrow(base_did)))
base_did$y2 = base_did$y + base_did$xe
iv_fmls = list(
  y2 ~ i(period, treat, ref = 5) | id | xe ~ z,
  y2 ~ x1 | id | xe ~ i(period, z, ref = 1),
  y2 ~ x1 | id | i(period, xe, keep = 6:7) ~ i(period, z, keep = 6:7)
)
for (fml_iv in iv_fmls) {
  fx_iv = coef(feols(fml_iv, data = base_did))
  names(fx_iv) = sub("^fit_", "", names(fx_iv))
  db_iv = coef(dbivreg(fml_iv, data = base_did, strategy = "demean"))
  expect_equal(db_iv[names(fx_iv)], fx_iv, tolerance = 1e-10, info = paste("dbivreg i():", deparse(fml_iv)))
}

# The same i() column across IV blocks is recognised as such
fml_dup = y2 ~ i(period, x1, ref = 1) | id | xe ~ z + i(period, x1, ref = 1)
expect_warning(db_dup <- dbivreg(fml_dup, data = base_did, strategy = "demean"), "period::2:x1")
fx_dup = coef(feols(y2 ~ i(period, x1, ref = 1) | id | xe ~ z, data = base_did))
names(fx_dup) = sub("^fit_", "", names(fx_dup))
expect_equal(coef(db_dup)[names(fx_dup)], fx_dup, tolerance = 1e-10, info = "i() instrument duplicating a regressor is dropped")
expect_error(
  dbivreg(y2 ~ i(period, x1, ref = 1) | id | i(period, x1, ref = 1) ~ i(period, z, ref = 1), data = base_did),
  "cannot also appear"
)

# ref/keep are evaluated in the formula's environment
fit_ref = function() {
  my_ref = 5
  dbreg(y ~ x1 + i(period, treat, ref = my_ref) | id, data = base_did, strategy = "demean")
}
expect_equal(
  coef(fit_ref()),
  coef(dbreg(y ~ x1 + i(period, treat, ref = 5) | id, data = base_did, strategy = "demean")),
  info = "i() ref can be a variable"
)

# Unsupported i() syntax errors informatively
expect_error(dbreg(y ~ i(period, treat, ref = 99), data = base_did), "match no level")
expect_error(dbreg(y ~ i(period, ref = "@^1"), data = base_did), "Regular expressions")
base_did$ts = as.POSIXct("2020-06-01", tz = "UTC") + 86400 * base_did$period
expect_error(dbreg(y ~ i(ts), data = base_did), "Date-time")
expect_error(dbreg(y ~ i(period, treat, bin = list(pre = 1:4)), data = base_did), "not supported yet: bin")
expect_error(dbreg(y ~ i(period, i.g), data = base_did), "i. prefix")
expect_error(dbreg(y ~ x1:i(period, ref = 5), data = base_did), "can't be interacted")
expect_error(dbreg(y ~ i(period, g), data = base_did), "numeric or logical")
