library(dbreg)
library(fixest)

rename_fixest_iv = function(x) {
  names(x) = sub("^fit_", "", names(x))
  x
}

extract_fixest_ivstat = function(stats, type, term) {
  key = paste0(type, "::", term)
  if (!is.null(stats[[key]])) {
    return(stats[[key]])
  }
  if (all(c("stat", "p", "df1", "df2") %in% names(stats))) {
    return(stats)
  }
  NULL
}

expect_iv_match = function(db_fit, fx_fit, tol_coef = 1e-6, tol_se = 1e-6, label = "IV") {
  fx_coef = rename_fixest_iv(coef(fx_fit))
  expect_true(
    max(abs(db_fit$coeftable[names(fx_coef), "estimate"] - fx_coef)) < tol_coef,
    info = paste(label, "coefficients match fixest")
  )

  fx_se = rename_fixest_iv(se(fx_fit))
  expect_true(
    max(abs(db_fit$coeftable[names(fx_se), "std.error"] - fx_se)) < tol_se,
    info = paste(label, "standard errors match fixest")
  )
}

# Hansen J from raw data: two-step efficient GMM, with the moment covariance
# estimated from 2SLS residuals, and J the minimised GMM criterion
manual_hansen_j = function(Z, X, y, cluster = NULL, weights = NULL) {
  w = if (is.null(weights)) rep(1, length(y)) else weights
  omega_from = function(u) {
    scores = Z * as.numeric(u * w)
    if (is.null(cluster)) crossprod(scores) else crossprod(rowsum(scores, cluster))
  }
  gmm = function(W) solve(t(X) %*% (w * Z) %*% W %*% t(Z) %*% (w * X), t(X) %*% (w * Z) %*% W %*% t(Z) %*% (w * y))
  b_2sls = gmm(solve(t(Z) %*% (w * Z)))
  W = solve(omega_from(y - X %*% b_2sls))
  m = t(Z) %*% (w * (y - X %*% gmm(W)))
  as.numeric(t(m) %*% W %*% m)
}

set.seed(2026)

tol = 1e-6
tol_cluster = 1e-5

n = 600L
cluster = factor(sample(1:60, n, replace = TRUE))
z = rnorm(n)
x = rnorm(n)
u = rnorm(n)
d = 0.9 * z + 0.5 * x + rnorm(n)
y = 1 + 2 * d + 0.3 * x + u
w = runif(n, min = 0.4, max = 2.2)

dat = data.frame(y = y, d = d, x = x, z = z, w = w, cluster = cluster)

db_iid = dbivreg(
  y ~ x | d ~ z,
  data = dat,
  weights = "w",
  vcov = "iid",
  strategy = "moments"
)
fx_iid = feols(y ~ x | d ~ z, data = dat, weights = ~w, vcov = "iid")
expect_iv_match(db_iid, fx_iid, tol_coef = tol, tol_se = tol, label = "No-FE IID")
fs_iid = fitstat(fx_iid, ~ ivf1 + sargan, simplify = TRUE)
expect_equal(
  db_iid$diagnostics$first_stage_f$d$stat,
  extract_fixest_ivstat(fs_iid, "ivf1", "d")$stat,
  tolerance = tol,
  info = "No-FE IID first-stage F matches fixest"
)
expect_true(
  is.na(db_iid$diagnostics$overid$stat) && is.na(fs_iid$sargan),
  info = "Exactly identified models report no Sargan statistic"
)

db_hc1 = dbivreg(
  y ~ x | d ~ z,
  data = dat,
  weights = "w",
  vcov = "hc1",
  strategy = "moments"
)
fx_hc1 = feols(y ~ x | d ~ z, data = dat, weights = ~w, vcov = "hc1")
expect_iv_match(db_hc1, fx_hc1, tol_coef = tol, tol_se = tol, label = "No-FE HC1")

db_cl = dbivreg(
  y ~ x | d ~ z,
  data = dat,
  weights = "w",
  vcov = ~cluster,
  strategy = "moments"
)
fx_cl = feols(y ~ x | d ~ z, data = dat, weights = ~w, vcov = ~cluster)
expect_iv_match(db_cl, fx_cl, tol_coef = tol, tol_se = tol_cluster, label = "No-FE cluster")
fs_cl = fitstat(fx_cl, ~ ivf1 + ivwald1, simplify = TRUE)
expect_equal(
  db_cl$diagnostics$first_stage_wald$d$stat,
  extract_fixest_ivstat(fs_cl, "ivwald1", "d")$stat,
  tolerance = tol_cluster,
  info = "No-FE cluster first-stage Wald matches fixest"
)

expect_true(
  dbivreg(
    y ~ x | d ~ z,
    data = dat,
    weights = "w",
    vcov = "iid",
    strategy = "auto"
  )$strategy == "moments",
  info = "Auto strategy selects moments when no fixed effects are present"
)

set.seed(2034)

n_comp_cells = 80L
comp_reps = sample(2:6, n_comp_cells, replace = TRUE)
dat_comp_cell = data.frame(
  cell = seq_len(n_comp_cells),
  cluster = factor(sample(1:20, n_comp_cells, replace = TRUE))
)
dat_comp_cell$x = round(rnorm(n_comp_cells), 2)
dat_comp_cell$z = round(rnorm(n_comp_cells), 2)
v_comp_cell = rnorm(n_comp_cells)
dat_comp_cell$d = round(0.8 * dat_comp_cell$z + 0.4 * dat_comp_cell$x + v_comp_cell, 2)

dat_comp = dat_comp_cell[rep(seq_len(n_comp_cells), comp_reps), ]
rownames(dat_comp) = NULL
dat_comp$w = runif(nrow(dat_comp), min = 0.5, max = 2.0)
dat_comp$y = 1.0 + 2.0 * dat_comp$d + 0.3 * dat_comp$x +
  0.6 * v_comp_cell[dat_comp$cell] + rnorm(nrow(dat_comp))

db_comp_hc1 = dbivreg(
  y ~ x | d ~ z,
  data = dat_comp,
  weights = "w",
  vcov = "hc1",
  strategy = "compress"
)
fx_comp_hc1 = feols(y ~ x | d ~ z, data = dat_comp, weights = ~w, vcov = "hc1")
expect_iv_match(db_comp_hc1, fx_comp_hc1, tol_coef = 1e-10, tol_se = 1e-10, label = "Compressed IV HC1")
expect_true(
  db_comp_hc1$nobs < db_comp_hc1$nobs_orig && db_comp_hc1$compression_ratio < 1,
  info = "Compressed IV records fewer design rows than original observations"
)
fs_comp_hc1 = fitstat(fx_comp_hc1, ~ ivwald1, simplify = TRUE)
expect_equal(
  db_comp_hc1$diagnostics$first_stage_wald$d$stat,
  extract_fixest_ivstat(fs_comp_hc1, "ivwald1", "d")$stat,
  tolerance = tol,
  info = "Compressed IV first-stage Wald matches fixest"
)

db_comp_cl = dbivreg(
  y ~ x | d ~ z,
  data = dat_comp,
  weights = "w",
  vcov = ~cluster,
  strategy = "compress"
)
fx_comp_cl = feols(y ~ x | d ~ z, data = dat_comp, weights = ~w, vcov = ~cluster)
expect_iv_match(db_comp_cl, fx_comp_cl, tol_coef = 1e-10, tol_se = 1e-10, label = "Compressed IV cluster")

sql_comp = invisible(capture.output(
  dbivreg(
    y ~ x | d ~ z,
    data = dat_comp,
    strategy = "compress",
    sql_only = TRUE
  )
))
expect_true(any(grepl("GROUP BY", sql_comp)), info = "Compressed IV sql_only exposes the GROUP BY compression")
expect_stdout(print(db_comp_hc1), "Compressed 2SLS estimation", info = "Compressed IV print header")

# Unweighted, overidentified compressed IV (exact on cells, so tight tolerances)
set.seed(2031)
n_ov_cells = 300L
ov_cell = data.frame(
  x = round(rnorm(n_ov_cells), 1),
  z1 = round(rnorm(n_ov_cells), 1),
  z2 = sample(0:3, n_ov_cells, replace = TRUE)
)
v_ov = rnorm(n_ov_cells)
ov_cell$d = round(ov_cell$z1 + 0.5 * ov_cell$z2 + v_ov, 1)
dat_comp_ov = ov_cell[rep(seq_len(n_ov_cells), 5), ]
dat_comp_ov$y = 1 + 2 * dat_comp_ov$d + 0.3 * dat_comp_ov$x + v_ov[rep(seq_len(n_ov_cells), 5)] + rnorm(nrow(dat_comp_ov))
dat_comp_ov$cl = sample(1:40, nrow(dat_comp_ov), replace = TRUE)

for (vc in list("iid", "hc1", ~cl)) {
  lbl = paste("Unweighted compressed IV", format(vc))
  db_ov = dbivreg(y ~ x | d ~ z1 + z2, data = dat_comp_ov, vcov = vc, strategy = "compress")
  fx_ov = feols(y ~ x | d ~ z1 + z2, data = dat_comp_ov, vcov = vc)
  expect_iv_match(db_ov, fx_ov, tol_coef = 1e-10, tol_se = 1e-10, label = lbl)
  # Diagnostics only use the cell-based meat under robust/clustered SEs
  if (identical(vc, "iid")) next
  db_ov_mom = dbivreg(y ~ x | d ~ z1 + z2, data = dat_comp_ov, vcov = vc, strategy = "moments")
  expect_equal(
    db_ov$diagnostics$first_stage_wald$d$stat,
    extract_fixest_ivstat(fitstat(fx_ov, ~ivwald1, simplify = TRUE), "ivwald1", "d")$stat,
    tolerance = 1e-10,
    info = paste(lbl, "first-stage Wald matches fixest")
  )
  expect_equal(
    db_ov$diagnostics$overid$stat,
    db_ov_mom$diagnostics$overid$stat,
    tolerance = 1e-6,
    info = paste(lbl, "Hansen J matches moments strategy")
  )
}

set.seed(2027)

n_fe = 700L
dat_fe1 = data.frame(
  fe1 = factor(sample(1:25, n_fe, replace = TRUE)),
  cluster = factor(sample(1:35, n_fe, replace = TRUE))
)

alpha = rnorm(25)[dat_fe1$fe1]
dat_fe1$z = rnorm(n_fe)
dat_fe1$x = rnorm(n_fe)
v = rnorm(n_fe)
dat_fe1$d = 0.8 * dat_fe1$z + 0.4 * dat_fe1$x + alpha + v
dat_fe1$y = 2.0 * dat_fe1$d + 0.3 * dat_fe1$x + alpha + 0.6 * v + rnorm(n_fe)
dat_fe1$w = runif(n_fe, min = 0.5, max = 2.0)

db_fe1 = dbivreg(
  y ~ x | fe1 | d ~ z,
  data = dat_fe1,
  weights = "w",
  vcov = "hc1",
  strategy = "demean"
)
fx_fe1 = feols(y ~ x | fe1 | d ~ z, data = dat_fe1, weights = ~w, vcov = "hc1")
expect_iv_match(db_fe1, fx_fe1, tol_coef = tol, tol_se = tol, label = "One-way FE HC1")

# Cluster on the FE itself (FE nested in cluster): ssc controls whether the
# nested FE levels count towards K in the CR1 correction
db_fe1_nested = dbivreg(
  y ~ x | fe1 | d ~ z,
  data = dat_fe1,
  vcov = ~fe1,
  ssc = "nested",
  strategy = "demean"
)
fx_fe1_nested = feols(y ~ x | fe1 | d ~ z, data = dat_fe1, vcov = ~fe1)
expect_iv_match(db_fe1_nested, fx_fe1_nested, tol_coef = tol, tol_se = tol_cluster, label = "One-way FE nested cluster")
expect_equal(
  db_fe1_nested$diagnostics$first_stage_wald$d$stat,
  extract_fixest_ivstat(fitstat(fx_fe1_nested, ~ivwald1, simplify = TRUE), "ivwald1", "d")$stat,
  tolerance = tol_cluster,
  info = "One-way FE nested cluster first-stage Wald matches fixest"
)

db_fe1_full = dbivreg(
  y ~ x | fe1 | d ~ z,
  data = dat_fe1,
  vcov = ~fe1,
  ssc = "full",
  strategy = "demean"
)
fx_fe1_full = feols(y ~ x | fe1 | d ~ z, data = dat_fe1, vcov = ~fe1, ssc = ssc(K.fixef = "full"))
expect_iv_match(db_fe1_full, fx_fe1_full, tol_coef = tol, tol_se = tol_cluster, label = "One-way FE full-K cluster")

db_fe1_within = dbivreg(
  y ~ x | fe1 | d ~ z,
  data = dat_fe1,
  weights = "w",
  vcov = "iid",
  strategy = "within"
)
fx_fe1_iid = feols(y ~ x | fe1 | d ~ z, data = dat_fe1, weights = ~w, vcov = "iid")
expect_iv_match(db_fe1_within, fx_fe1_iid, tol_coef = tol, tol_se = tol, label = "Within alias")
expect_true(db_fe1_within$strategy == "demean", info = "Within alias normalizes to demean")

expect_true(
  dbivreg(
    y ~ x | fe1 | d ~ z,
    data = dat_fe1,
    weights = "w",
    vcov = "iid",
    strategy = "auto"
  )$strategy == "demean",
  info = "Auto strategy selects demean when fixed effects are present"
)

set.seed(2028)

n_units = 20L
n_time = 5L
dat_fe2 = expand.grid(unit = 1:n_units, time = 1:n_time)
dat_fe2$fe1 = factor(dat_fe2$unit)
dat_fe2$fe2 = factor(dat_fe2$time)

alpha_2 = rnorm(n_units)[dat_fe2$unit]
tau_2 = rnorm(n_time)[dat_fe2$time]
dat_fe2$z = rnorm(nrow(dat_fe2))
dat_fe2$x = rnorm(nrow(dat_fe2))
v_2 = rnorm(nrow(dat_fe2))
dat_fe2$d = 0.9 * dat_fe2$z + 0.4 * dat_fe2$x + alpha_2 + tau_2 + v_2
dat_fe2$y = 2.0 * dat_fe2$d + 0.3 * dat_fe2$x + alpha_2 + tau_2 + 0.5 * v_2 + rnorm(nrow(dat_fe2))

db_fe2 = dbivreg(
  y ~ x | fe1 + fe2 | d ~ z,
  data = dat_fe2,
  vcov = "iid",
  strategy = "demean"
)
fx_fe2 = feols(y ~ x | fe1 + fe2 | d ~ z, data = dat_fe2, vcov = "iid")
expect_iv_match(db_fe2, fx_fe2, tol_coef = tol, tol_se = tol, label = "Two-way FE balanced IID")
fs_fe2 = fitstat(fx_fe2, ~ ivf1 + ivwald1, simplify = TRUE)
expect_equal(
  db_fe2$diagnostics$first_stage_f$d$stat,
  extract_fixest_ivstat(fs_fe2, "ivf1", "d")$stat,
  tolerance = tol,
  info = "Two-way FE first-stage F matches fixest"
)

set.seed(2029)

n_units_ap = 30L
n_time_ap = 6L
dat_ap = expand.grid(unit = 1:n_units_ap, time = 1:n_time_ap)
dat_ap$fe1 = factor(dat_ap$unit)
dat_ap$fe2 = factor(dat_ap$time)
dat_ap$cluster = factor(sample(1:30, nrow(dat_ap), replace = TRUE))

alpha_ap = rnorm(n_units_ap)[dat_ap$unit]
tau_ap = rnorm(n_time_ap)[dat_ap$time]
dat_ap$z = rnorm(nrow(dat_ap))
dat_ap$x = rnorm(nrow(dat_ap))
v_ap = rnorm(nrow(dat_ap))
dat_ap$d = 0.8 * dat_ap$z + 0.5 * dat_ap$x + alpha_ap + tau_ap + v_ap
dat_ap$y = 2.0 * dat_ap$d + 0.3 * dat_ap$x + alpha_ap + tau_ap + 0.6 * v_ap + rnorm(nrow(dat_ap))
dat_ap$w = runif(nrow(dat_ap), min = 0.5, max = 2.0)

drop_idx = sample(seq_len(nrow(dat_ap)), size = round(0.2 * nrow(dat_ap)))
dat_ap = dat_ap[-drop_idx, ]

db_ap = dbivreg(
  y ~ x | fe1 + fe2 | d ~ z,
  data = dat_ap,
  weights = "w",
  vcov = ~cluster,
  strategy = "demean"
)
fx_ap = feols(y ~ x | fe1 + fe2 | d ~ z, data = dat_ap, weights = ~w, vcov = ~cluster)
expect_iv_match(db_ap, fx_ap, tol_coef = tol, tol_se = tol_cluster, label = "Two-way FE AP cluster")
fs_ap = fitstat(fx_ap, ~ ivf1 + ivwald1, simplify = TRUE)
expect_equal(
  db_ap$diagnostics$first_stage_wald$d$stat,
  extract_fixest_ivstat(fs_ap, "ivwald1", "d")$stat,
  tolerance = tol_cluster,
  info = "Two-way FE cluster first-stage Wald matches fixest"
)

db_ap_nested = dbivreg(
  y ~ x | fe1 + fe2 | d ~ z,
  data = dat_ap,
  weights = "w",
  vcov = ~fe1,
  ssc = "nested",
  strategy = "demean"
)
fx_ap_nested = feols(y ~ x | fe1 + fe2 | d ~ z, data = dat_ap, weights = ~w, vcov = ~fe1)
expect_iv_match(db_ap_nested, fx_ap_nested, tol_coef = tol, tol_se = tol_cluster, label = "Two-way FE AP nested cluster")

set.seed(2033)

n_units_fe3 = 18L
n_time_fe3 = 4L
n_market_fe3 = 3L
dat_fe3 = expand.grid(unit = 1:n_units_fe3, time = 1:n_time_fe3, market = 1:n_market_fe3)
dat_fe3$fe1 = factor(dat_fe3$unit)
dat_fe3$fe2 = factor(dat_fe3$time)
dat_fe3$fe3 = factor(dat_fe3$market)

alpha_3 = rnorm(n_units_fe3)[dat_fe3$unit]
tau_3 = rnorm(n_time_fe3)[dat_fe3$time]
gamma_3 = rnorm(n_market_fe3)[dat_fe3$market]
dat_fe3$z = rnorm(nrow(dat_fe3))
dat_fe3$x = rnorm(nrow(dat_fe3))
v_3 = rnorm(nrow(dat_fe3))
dat_fe3$d = 0.8 * dat_fe3$z + 0.4 * dat_fe3$x + alpha_3 + tau_3 + gamma_3 + v_3
dat_fe3$y = 1.9 * dat_fe3$d + 0.25 * dat_fe3$x + alpha_3 + tau_3 + gamma_3 +
  0.5 * v_3 + rnorm(nrow(dat_fe3))

db_fe3 = dbivreg(
  y ~ x | fe1 + fe2 + fe3 | d ~ z,
  data = dat_fe3,
  vcov = "iid",
  strategy = "demean"
)
fx_fe3 = feols(y ~ x | fe1 + fe2 + fe3 | d ~ z, data = dat_fe3, vcov = "iid")
expect_iv_match(db_fe3, fx_fe3, tol_coef = tol, tol_se = tol, label = "Three-way FE AP IID")
fs_fe3 = fitstat(fx_fe3, ~ ivf1, simplify = TRUE)
expect_equal(
  db_fe3$diagnostics$first_stage_f$d$stat,
  extract_fixest_ivstat(fs_fe3, "ivf1", "d")$stat,
  tolerance = tol,
  info = "Three-way FE first-stage F matches fixest"
)
expect_equal(
  db_fe3$n_fe_levels,
  c(fe1 = n_units_fe3, fe2 = n_time_fe3, fe3 = n_market_fe3),
  info = "Three-way FE levels are retained in the fitted object"
)
expect_equal(
  db_fe3$n_fe3,
  n_market_fe3,
  info = "Third fixed-effect count is available by position"
)

set.seed(2032)

n_over = 700L
dat_over = data.frame(
  x = rnorm(n_over),
  z1 = rnorm(n_over),
  z2 = rnorm(n_over),
  cluster = factor(sample(1:50, n_over, replace = TRUE))
)
v_over = rnorm(n_over)
dat_over$d = 0.8 * dat_over$z1 + 0.3 * dat_over$z2 + 0.4 * dat_over$x + v_over
dat_over$y = 1.0 + 2.0 * dat_over$d + 0.4 * dat_over$x + 0.6 * v_over + rnorm(n_over)

db_over = dbivreg(
  y ~ x | d ~ z1 + z2,
  data = dat_over,
  vcov = "iid",
  strategy = "moments"
)
fx_over = feols(y ~ x | d ~ z1 + z2, data = dat_over, vcov = "iid")
expect_iv_match(db_over, fx_over, tol_coef = tol, tol_se = tol, label = "Overidentified IID")
fs_over = fitstat(fx_over, ~ ivf1 + sargan, simplify = TRUE)
expect_equal(
  db_over$diagnostics$first_stage_f$d$stat,
  extract_fixest_ivstat(fs_over, "ivf1", "d")$stat,
  tolerance = tol,
  info = "Overidentified first-stage F matches fixest"
)
expect_equal(
  db_over$diagnostics$overid$stat,
  fs_over$sargan$stat,
  tolerance = tol,
  info = "Sargan statistic matches fixest"
)
expect_equal(
  db_over$diagnostics$overid$p,
  fs_over$sargan$p,
  tolerance = tol,
  info = "Sargan p-value matches fixest"
)
expect_true(
  identical(db_over$diagnostics$overid$test, "Sargan"),
  info = "IID overidentification test is labeled Sargan"
)

db_over_hc1 = dbivreg(
  y ~ x | d ~ z1 + z2,
  data = dat_over,
  vcov = "hc1",
  strategy = "moments"
)
Z_over = cbind("(Intercept)" = 1, x = dat_over$x, z1 = dat_over$z1, z2 = dat_over$z2)
X_over = cbind("(Intercept)" = 1, x = dat_over$x, d = dat_over$d)
expect_equal(
  db_over_hc1$diagnostics$overid$stat,
  manual_hansen_j(Z_over, X_over, dat_over$y),
  tolerance = tol,
  info = "HC1 Hansen J matches manual calculation"
)
expect_true(
  identical(db_over_hc1$diagnostics$overid$test, "Hansen J"),
  info = "HC1 overidentification test is labeled Hansen J"
)

db_over_cl = dbivreg(
  y ~ x | d ~ z1 + z2,
  data = dat_over,
  vcov = ~cluster,
  strategy = "moments"
)
expect_equal(
  db_over_cl$diagnostics$overid$stat,
  manual_hansen_j(Z_over, X_over, dat_over$y, cluster = dat_over$cluster),
  tolerance = tol_cluster,
  info = "Cluster-robust Hansen J matches manual calculation"
)

set.seed(2030)

n_fac = 900L
dat_fac = data.frame(
  g = factor(sample(letters[1:3], n_fac, replace = TRUE))
)
g_b = as.numeric(dat_fac$g == "b")
g_c = as.numeric(dat_fac$g == "c")
dat_fac$x = rnorm(n_fac)
dat_fac$z = rnorm(n_fac)
v_fac = rnorm(n_fac)
dat_fac$d = 0.8 * dat_fac$z + 0.4 * dat_fac$x + 0.6 * g_b - 0.3 * g_c +
  0.5 * dat_fac$z * g_b - 0.4 * dat_fac$z * g_c + v_fac
dat_fac$y = 1.0 + 1.8 * dat_fac$d + 0.9 * dat_fac$d * g_b - 0.7 * dat_fac$d * g_c +
  0.3 * dat_fac$x + 0.5 * g_b - 0.2 * g_c + 0.7 * v_fac + rnorm(n_fac)

db_fac = dbivreg(
  y ~ x + g | d + d:g ~ z + z:g,
  data = dat_fac,
  vcov = "hc1",
  strategy = "moments"
)
fx_fac = feols(y ~ x + g | d + d:g ~ z + z:g, data = dat_fac, vcov = "hc1")
expect_iv_match(db_fac, fx_fac, tol_coef = tol, tol_se = tol, label = "Factor-expanded IV moments")
fs_fac = fitstat(fx_fac, ~ ivf1 + ivwald1, simplify = TRUE)
for (nm in names(db_fac$diagnostics$first_stage_wald)) {
  expect_equal(
    db_fac$diagnostics$first_stage_wald[[nm]]$stat,
    extract_fixest_ivstat(fs_fac, "ivwald1", nm)$stat,
    tolerance = tol_cluster,
    info = paste("Factor-expanded first-stage Wald matches fixest for", nm)
  )
}

sql_fac = invisible(capture.output(
  dbivreg(
    y ~ x + g | d + d:g ~ z + z:g,
    data = dat_fac,
    sql_only = TRUE
  )
))
expect_true(any(grepl("CASE WHEN", sql_fac)), info = "sql_only expands factor IV terms in SQL")

set.seed(2031)

n_fac_fe = 850L
dat_fac_fe = data.frame(
  g = factor(sample(letters[1:3], n_fac_fe, replace = TRUE)),
  fe = factor(sample(1:30, n_fac_fe, replace = TRUE))
)
gfe_b = as.numeric(dat_fac_fe$g == "b")
gfe_c = as.numeric(dat_fac_fe$g == "c")
alpha_fe = rnorm(30)[dat_fac_fe$fe]
dat_fac_fe$x = rnorm(n_fac_fe)
dat_fac_fe$z = rnorm(n_fac_fe)
v_fac_fe = rnorm(n_fac_fe)
dat_fac_fe$d = 0.7 * dat_fac_fe$z + 0.3 * dat_fac_fe$x + 0.5 * gfe_b - 0.2 * gfe_c +
  0.4 * dat_fac_fe$z * gfe_b - 0.3 * dat_fac_fe$z * gfe_c + alpha_fe + v_fac_fe
dat_fac_fe$y = 1.5 * dat_fac_fe$d + 0.7 * dat_fac_fe$d * gfe_b - 0.5 * dat_fac_fe$d * gfe_c +
  0.4 * dat_fac_fe$x + 0.4 * gfe_b - 0.1 * gfe_c + alpha_fe + 0.6 * v_fac_fe + rnorm(n_fac_fe)

db_fac_fe = dbivreg(
  y ~ x + g | fe | d + d:g ~ z + z:g,
  data = dat_fac_fe,
  vcov = "iid",
  strategy = "demean"
)
fx_fac_fe = feols(y ~ x + g | fe | d + d:g ~ z + z:g, data = dat_fac_fe, vcov = "iid")
expect_iv_match(db_fac_fe, fx_fac_fe, tol_coef = tol, tol_se = tol, label = "Factor-expanded IV demean")

expect_error(
  dbivreg(y ~ x | d ~ z, data = dat, endog = ~d, instruments = ~z),
  "no longer supported"
)

expect_error(
  dbivreg(y ~ x | fe1, data = dat_fe1),
  "fixest-style IV formula|IV specification|IV block"
)

expect_error(
  dbivreg(y ~ x | d ~ z, data = dat, strategy = "mundlak"),
  "does not yet implement strategy = 'mundlak'"
)

expect_error(
  dbivreg(y ~ x | fe1 | d ~ z, data = dat_fe1, strategy = "compress"),
  "only available without fixed effects"
)

expect_error(
  dbivreg(y ~ x | fe1 | d ~ z, data = dat_fe1, strategy = "moments"),
  "only available without fixed effects"
)

expect_error(
  dbivreg(y ~ x | d ~ z, data = dat, strategy = "demean"),
  "requires at least one fixed effect"
)

# "auto" strategy selection is shared with dbreg()
set.seed(2040)
auto_cell = data.frame(x = sample(1:5, 300, TRUE), z1 = sample(0:3, 300, TRUE), z2 = sample(0:3, 300, TRUE))
auto_cell$d = auto_cell$z1 + auto_cell$z2 + sample(0:1, 300, TRUE)
dat_auto = auto_cell[rep(seq_len(300), 50), ]
dat_auto$y = dat_auto$d + dat_auto$x + rnorm(nrow(dat_auto))
dat_auto$fe = sample(1:10, nrow(dat_auto), TRUE)
expect_equal(dbivreg(y ~ x | d ~ z1 + z2, data = dat_auto)$strategy, "compress", info = "auto picks compress for discrete no-FE IV")
expect_equal(dbivreg(y ~ x | d ~ z1 + z2, data = transform(dat_auto, z1 = rnorm(nrow(dat_auto))))$strategy, "moments", info = "auto avoids compress with continuous instruments")
expect_equal(dbivreg(y ~ x | d ~ z1 + z2, data = dat_auto, compress_nmax = 10)$strategy, "moments", info = "auto respects compress_nmax")
expect_equal(dbivreg(y ~ x | fe | d ~ z1 + z2, data = dat_auto)$strategy, "demean", info = "auto uses demean with FE (no FE compress for IV)")

# Unsupported specifications are rejected rather than silently changed
expect_error(dbivreg(log(y) ~ x | d ~ z, data = dat), "outcome", info = "transformed outcome rejected")
expect_error(dbivreg(y ~ log(abs(x)) | d ~ z, data = dat), "exogenous regressors", info = "transformed regressor rejected")
expect_error(dbivreg(y ~ x | d ~ I(z^2), data = dat), "instruments", info = "transformed instrument rejected")

# Singular diagnostics are reported as unavailable instead of aborting the fit
dat_sing = dat
dat_sing$d = dat_sing$z
expect_warning(
  db_sing <- dbivreg(y ~ x | d ~ z, data = dat_sing),
  "First-stage Wald test unavailable",
  info = "singular first-stage covariance warns"
)
expect_true(is.na(db_sing$diagnostics$first_stage_wald$d$stat), info = "singular first-stage Wald is NA")
expect_true(all(is.finite(db_sing$coeftable[, "estimate"])), info = "fit survives singular diagnostics")

# predict() with factor-expanded structural terms
dat_pred = dat
dat_pred$g = sample(c("a", "b", "c"), nrow(dat_pred), replace = TRUE)
expect_equal(
  as.numeric(predict(dbivreg(y ~ x + g | d ~ z, data = dat_pred), newdata = dat_pred)),
  as.numeric(predict(feols(y ~ x + g | d ~ z, data = dat_pred), newdata = dat_pred)),
  tolerance = 1e-10,
  info = "predict matches fixest with factor terms"
)

# Failed validation releases the registered in-memory table
con_val = DBI::dbConnect(duckdb::duckdb())
tables_before = DBI::dbListTables(con_val)
expect_error(dbivreg(y ~ x | d ~ z, conn = con_val, data = dat, weights = "not_a_column"))
expect_equal(DBI::dbListTables(con_val), tables_before, info = "no registered table left behind after a validation error")
DBI::dbDisconnect(con_val, shutdown = TRUE)
con_val = DBI::dbConnect(duckdb::duckdb())
expect_error(dbivreg(y ~ x | d ~ z, conn = con_val, data = dat, compress_ratio = 2), "compress_ratio")
expect_equal(DBI::dbListTables(con_val), character(0), info = "no registered table left behind after a late validation error")
DBI::dbDisconnect(con_val, shutdown = TRUE)

# predict() refuses demean models with more than two FE (would omit the rest)
expect_error(
  predict(db_fe3, newdata = dat_fe3),
  "more than two fixed effects",
  info = "IV predict with 3 FE errors"
)

# No-intercept IV models (#68)
expect_iv_match(
  dbivreg(y ~ 0 + x + g | d ~ z, data = dat_pred),
  feols(y ~ 0 + x + g | d ~ z, data = dat_pred),
  label = "No-intercept IV (first factor keeps all levels)"
)
expect_iv_match(
  dbivreg(y ~ 0 + x | d ~ z1 + z2, data = dat_comp_ov, strategy = "compress"),
  feols(y ~ 0 + x | d ~ z1 + z2, data = dat_comp_ov),
  tol_coef = 1e-10, tol_se = 1e-10,
  label = "No-intercept compressed IV"
)
expect_error(
  dbivreg(y ~ 0 + x | g ~ z, data = dat_pred),
  "factor endogenous",
  info = "no-intercept with factor endogenous rejected"
)
