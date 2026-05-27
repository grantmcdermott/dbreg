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

manual_hansen_j = function(Z, u, cluster = NULL, weights = NULL) {
  if (is.null(weights)) {
    weights = rep(1, length(u))
  }
  scores = Z * as.numeric(u * weights)
  g = colSums(scores)

  if (is.null(cluster)) {
    omega = crossprod(scores)
  } else {
    omega = matrix(0, ncol(scores), ncol(scores))
    for (cl in unique(cluster)) {
      s_g = colSums(scores[cluster == cl, , drop = FALSE])
      omega = omega + tcrossprod(s_g)
    }
  }

  as.numeric(t(g) %*% qr.solve(omega, g))
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
expect_iv_match(db_comp_hc1, fx_comp_hc1, tol_coef = tol, tol_se = tol, label = "Compressed IV HC1")
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
expect_iv_match(db_comp_cl, fx_comp_cl, tol_coef = tol, tol_se = tol_cluster, label = "Compressed IV cluster")

sql_comp = invisible(capture.output(
  dbivreg(
    y ~ x | d ~ z,
    data = dat_comp,
    strategy = "compress",
    sql_only = TRUE
  )
))
expect_true(any(grepl("compressed AS", sql_comp)), info = "Compressed IV sql_only exposes compressed CTE")

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
u_over_hc1 = as.numeric(dat_over$y - X_over %*% coef(db_over_hc1)[colnames(X_over)])
expect_equal(
  db_over_hc1$diagnostics$overid$stat,
  manual_hansen_j(Z_over, u_over_hc1),
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
u_over_cl = as.numeric(dat_over$y - X_over %*% coef(db_over_cl)[colnames(X_over)])
expect_equal(
  db_over_cl$diagnostics$overid$stat,
  manual_hansen_j(Z_over, u_over_cl, cluster = dat_over$cluster),
  tolerance = tol_cluster,
  info = "Cluster-robust Hansen J matches manual calculation"
)
expect_true(
  identical(db_over_cl$diagnostics$overid$test, "Hansen J"),
  info = "Cluster overidentification test is labeled Hansen J"
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
