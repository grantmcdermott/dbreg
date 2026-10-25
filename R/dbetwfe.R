#' Extended two-way fixed effects (ETWFE) on database backends
#'
#' @md
#' @description
#' Estimates heterogeneous treatment effects for staggered adoption designs
#' with the extended two-way fixed effects (ETWFE) approach of Wooldridge
#' (2021), as implemented by the \pkg{etwfe} package, on data that live in a
#' database. The regression includes cohort and period fixed effects (the
#' Mundlak equivalent of unit and period fixed effects) plus a separate
#' treatment effect for every treated cohort-by-period cell. These
#' cohort-by-period effects are then aggregated, e.g. by event time for an
#' event study.
#'
#' Because every regressor is a function of the cohort and the period, the
#' regression only needs one row per cohort-by-period cell, which `dbetwfe`
#' computes with a single `GROUP BY` in the database. Results match
#' `etwfe::etwfe()` followed by `etwfe::emfx()`.
#'
#' @param fml A formula with the outcome on the left-hand side, e.g. `y ~ 0`.
#'   Control variables are not supported yet.
#' @param tvar,gvar Time (period) and group (treatment cohort) variables, as
#'   unquoted names or strings. Both must be numeric, with `gvar` giving the
#'   first treated period of each unit. Never-treated units can be coded as
#'   `NULL`/`NA`, or by a value outside the observed periods (e.g. `0`).
#' @param conn,table,data,path Data source, as in [dbreg()].
#' @param cgroup Control group: `"notyet"` (not-yet-treated and never-treated
#'   units, the default) or `"never"` (never-treated units only). With
#'   `"never"`, pre-treatment effects are estimated too, relative to the
#'   period before treatment.
#' @param tref,gref Optional reference period and reference (never-treated)
#'   cohort. By default `tref` is the first period and `gref` is detected as
#'   in \pkg{etwfe}.
#' @param aggregate How to aggregate the cohort-by-period treatment effects:
#'   `"event"` (by time since treatment, the default), `"simple"` (a single
#'   overall ATT), `"group"` (by cohort), `"calendar"` (by period), or
#'   `"none"` (the raw cohort-by-period effects). Aggregates are averages
#'   weighted by the number of treated observations, as in `etwfe::emfx()`.
#'   See [aggregate.dbetwfe()] to re-aggregate a fitted model.
#' @param unconditional Logical. Unconditional standard errors for the
#'   aggregates (the default, as in `etwfe::emfx()`)? See
#'   [aggregate.dbetwfe()].
#' @param post_only Logical. For `cgroup = "notyet"` and
#'   `aggregate = "event"`, report only post-treatment event times? If
#'   `FALSE`, pre-treatment event times are shown with (mechanical) zero
#'   effects.
#' @param window Optional event-time window for `aggregate = "event"`: a single
#'   number (symmetric) or two numbers (periods before and after treatment).
#' @param weights,vcov,cluster,drop_missings,verbose As in [dbreg()].
#' @param ... Currently ignored.
#' @return An object of class `dbetwfe` (inheriting from `dbreg`), whose
#'   coefficient table holds the aggregated treatment effects. The underlying
#'   cohort-by-period effects are kept, so [aggregate.dbetwfe()] can
#'   re-aggregate without querying the data again.
#' @references
#' Wooldridge, Jeffrey M. (2021). Two-Way Fixed Effects, the Two-Way Mundlak
#' Regression, and Difference-in-Differences Estimators. Working paper.
#' \doi{10.2139/ssrn.3906345}
#' @seealso [aggregate.dbetwfe()], [dbreg()]
#' @importFrom stats aggregate
#' @export
#' @examples
#' if (requireNamespace("fixest", quietly = TRUE)) {
#'   data("base_stagg", package = "fixest")
#'
#'   # Event study (cohort-by-period effects aggregated by event time)
#'   es = dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg)
#'   es
#'   plot(es)
#'
#'   # Other aggregations of the same fit, without re-querying the data
#'   aggregate(es, "simple")
#'   aggregate(es, "group")
#'
#'   # Never-treated controls (also estimates pre-treatment effects) and
#'   # clustered standard errors
#'   dbetwfe(
#'     y ~ 0, tvar = year, gvar = year_treated, data = base_stagg,
#'     cgroup = "never", vcov = ~id
#'   )
#' }
dbetwfe = function(
  fml,
  tvar,
  gvar,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  cgroup = c("notyet", "never"),
  tref = NULL,
  gref = NULL,
  aggregate = c("event", "simple", "group", "calendar", "none"),
  post_only = TRUE,
  window = NULL,
  unconditional = TRUE,
  weights = NULL,
  vcov = c("iid", "hc1"),
  cluster = NULL,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  if (missing(tvar) || missing(gvar)) {
    stop("Both `tvar` and `gvar` are required.", call. = FALSE)
  }
  tvar = column_arg(substitute(tvar), parent.frame(), "tvar")
  gvar = column_arg(substitute(gvar), parent.frame(), "gvar")
  cgroup = match.arg(cgroup)
  aggregate = match.arg(aggregate)
  verbose = isTRUE(verbose)
  vcov_parsed = parse_vcov_args(vcov, cluster)
  vcov_type_req = vcov_parsed[["vcov_type"]]
  cluster_var = vcov_parsed[["cluster_var"]]

  inputs = new.env(parent = emptyenv())
  env2env(
    environment(),
    inputs,
    c(
      "fml", "tvar", "gvar", "conn", "table", "data", "path", "cgroup", "tref",
      "gref", "weights", "vcov_type_req", "cluster_var", "drop_missings", "verbose"
    )
  )
  on.exit(release_db_setup(inputs[["conn"]], inputs[["own_conn"]], inputs[["registered_table"]]), add = TRUE)

  process_dbetwfe_inputs(inputs)
  fit = fit_dbetwfe(inputs)
  fit = restore_result_names(fit, inputs)
  class(fit) = c("dbetwfe", "dbreg")
  aggregate.dbetwfe(fit, aggregate, post_only = post_only, window = window, unconditional = unconditional)
}

# A column given as an unquoted name or a string (or an expression giving one)
column_arg = function(e, env, arg) {
  if (is.name(e)) {
    return(as.character(e))
  }
  val = eval(e, env)
  if (!is.character(val) || length(val) != 1) {
    stop("`", arg, "` must be a single column name.", call. = FALSE)
  }
  val
}

#' Validate dbetwfe() inputs and set up the data source
#' @keywords internal
process_dbetwfe_inputs = function(inputs) {
  env2env(
    inputs,
    environment(),
    c("fml", "tvar", "gvar", "conn", "table", "data", "path", "weights", "cluster_var", "drop_missings")
  )
  fml = Formula(fml)
  yvar = all.vars(formula(fml, lhs = 1, rhs = 0))
  if (length(yvar) != 1) {
    stop("Exactly one outcome variable required.", call. = FALSE)
  }
  if (length(fml)[2] > 1 || length(attr(terms(formula(fml, lhs = 0, rhs = 1)), "term.labels"))) {
    stop(
      "Control variables (and fixed effects) are not supported by dbetwfe() yet. ",
      "Use a formula like `y ~ 0`.",
      call. = FALSE
    )
  }

  db_setup = setup_db_connection(conn, table, data, path, caller = "dbetwfe")
  conn = db_setup[["conn"]]
  own_conn = db_setup[["own_conn"]]
  registered_table = db_setup[["registered_table"]]
  env2env(environment(), inputs, c("conn", "own_conn", "registered_table"))

  src = safe_source(conn, db_setup[["from_statement"]], c(yvar, tvar, gvar, weights, cluster_var))
  validate_weights(if (is.null(data)) src[["to_safe"]](weights) else weights, data, conn, src[["from_statement"]])
  yvar = src[["to_safe"]](yvar)
  tvar = src[["to_safe"]](tvar)
  gvar = src[["to_safe"]](gvar)
  weights = src[["to_safe"]](weights)
  cluster_var = src[["to_safe"]](cluster_var)
  name_map = src[["map"]]
  source_map = name_map
  # A missing cohort means never treated, so it doesn't drop the row
  from_statement = filter_estimation_sample(
    src[["from_statement"]],
    vars = c(yvar, tvar, cluster_var),
    weights = weights,
    drop_missings = drop_missings
  )
  env2env(
    environment(),
    inputs,
    c("yvar", "tvar", "gvar", "weights", "cluster_var", "from_statement", "name_map", "source_map")
  )
  invisible(inputs)
}

#' Apply etwfe's design rules to the cohort-by-period cells
#'
#' Determines the reference cohort and period, the treatment indicator
#' (`.Dtreat` in \pkg{etwfe}) and which cells get their own treatment effect.
#' @param g,t Cohort and period of each cell.
#' @return List with `g` (cohort, never-treated `NA` recoded to `gref`),
#'   `gref`, `tref`, `dtreat`, `keep` (cells in the estimation sample) and
#'   `att` (cells with their own treatment effect).
#' @keywords internal
etwfe_cells = function(g, t, cgroup, gref = NULL, tref = NULL, gvar = "gvar", tvar = "tvar") {
  if (!is.numeric(t) || !(is.numeric(g) || all(is.na(g)))) {
    stop("`tvar` and `gvar` must be numeric (e.g. years).", call. = FALSE)
  }
  ut = sort(unique(t))
  # Never-treated units may have a missing cohort
  if (anyNA(g)) {
    g[is.na(g)] = if (is.null(gref)) max(ut) + 1 else gref
  }
  ug = sort(unique(g))
  # Reference (never-treated) cohort, as in etwfe
  if (is.null(gref)) {
    gref = ug[ug > max(ut)]
    if (!length(gref)) gref = ug[ug < min(ut)]
    if (!length(gref) && cgroup == "notyet") gref = max(ug)
    if (!length(gref)) {
      stop(
        "The '", cgroup, "' control group for ", gvar, " could not be identified. ",
        "Supply a reference cohort via `gref`.",
        call. = FALSE
      )
    }
    gref = min(gref)
  } else if (!gref %in% ug) {
    stop("`gref` value ", gref, " not found in ", gvar, ".", call. = FALSE)
  }
  if (is.null(tref)) {
    tref = min(ut)
  } else if (!tref %in% ut) {
    stop("`tref` value ", tref, " not found in ", tvar, ".", call. = FALSE)
  }

  if (cgroup == "notyet") {
    dtreat = t >= g & g != gref
    # Once the reference cohort is treated, there are no controls left
    keep = if (gref < min(ut)) t > gref else t < gref
    att = dtreat & keep & t != tref
  } else {
    # Each treated cohort's reference is the period before treatment
    dtreat = t != g - 1
    keep = rep(TRUE, length(t))
    att = dtreat & g != gref
  }
  list(g = g, gref = gref, tref = tref, dtreat = dtreat, keep = keep, att = att)
}

#' Fit the ETWFE regression on cohort-by-period cells
#' @keywords internal
fit_dbetwfe = function(inputs) {
  env2env(
    inputs,
    environment(),
    c("conn", "yvar", "tvar", "gvar", "weights", "cluster_var", "from_statement", "cgroup", "vcov_type_req", "verbose")
  )
  # Cell sufficient statistics: one row per cohort-by-period cell
  moment_terms = build_weighted_moment_terms(
    y_sql = yvar,
    weights_expr = sql_weight_expr(weights),
    prefix_terms = "COUNT(*) AS n",
    include_w_sq = TRUE
  )
  query_string = paste0(
    "SELECT ", gvar, ", ", tvar, ", ", paste(moment_terms, collapse = ", "), "\n",
    from_statement, "\nGROUP BY ", gvar, ", ", tvar
  )
  if (verbose) {
    message("[dbreg] Executing dbetwfe cell query\n")
  }
  cells = dbGetQuery(conn, query_string)
  # Display names for labels
  gvar_disp = restore_names(gvar, inputs[["name_map"]])
  tvar_disp = restore_names(tvar, inputs[["name_map"]])
  design = etwfe_cells(cells[[gvar]], cells[[tvar]], cgroup, inputs[["gref"]], inputs[["tref"]], gvar_disp, tvar_disp)
  ord = order(design[["g"]], cells[[tvar]])
  cells = cells[ord, , drop = FALSE]
  cells[[".g"]] = design[["g"]][ord]
  cells[[".t"]] = cells[[tvar]]
  cells[[".dtreat"]] = design[["dtreat"]][ord]
  cells[[".att"]] = ifelse(design[["att"]][ord], paste0(".Dtreat:", gvar_disp, "::", cells[[".g"]], ":", tvar_disp, "::", cells[[".t"]]), NA)
  est = cells[design[["keep"]][ord], , drop = FALSE]
  if (!any(!is.na(est[[".att"]]))) {
    stop("No treated cohort-by-period cells to estimate effects for.", call. = FALSE)
  }

  # Design: intercept, cohort and period dummies, one dummy per treated cell
  X = model.matrix(~ factor(g) + factor(t), data.frame(g = est[[".g"]], t = est[[".t"]]))
  att_names = est[[".att"]][!is.na(est[[".att"]])]
  X_att = outer(est[[".att"]], att_names, function(a, b) as.numeric(!is.na(a) & a == b))
  colnames(X_att) = att_names
  X = cbind(X, X_att)

  # Weighted least squares on cell means (as in the compress strategy)
  sum_w = est[["sum_w"]]
  wts = sqrt(sum_w)
  Y = est[["sum_wy"]] / sum_w
  XtX = crossprod(X * wts)
  XtY = crossprod(X * wts, Y * wts)
  collin = detect_collinearity(XtX, XtY, verbose = verbose)
  X = X[, collin[["keep_names"]], drop = FALSE]
  sol = solve_with_fallback(collin[["XtX"]], collin[["Xty"]])
  betahat = sol[["betahat"]]
  rownames(betahat) = colnames(X)
  yhat = as.numeric(X %*% betahat)

  nobs_orig = sum(est[["n"]])
  df_res = max(nobs_orig - ncol(X), 1)
  rss_g = est[["sum_wy_sq"]] - 2 * yhat * est[["sum_wy"]] + sum_w * yhat^2
  rss = sum(rss_g)
  tss = sum(est[["sum_wy_sq"]]) - sum(est[["sum_wy"]])^2 / sum(sum_w)

  # Score moments, for the model's SEs and for unconditional aggregate SEs:
  # Suu = sum of score cross-products (the meat), Sun = scores x cell counts,
  # Snn = cell-count cross-products, summed by cluster (or observation)
  e_sum = est[["sum_wy"]] - sum_w * yhat
  e2_sum = if (is.null(weights)) rss_g else est[["sum_w2y_sq"]] - 2 * yhat * est[["sum_w2y"]] + est[["sum_w2"]] * yhat^2
  if (vcov_type_req == "cluster") {
    moments = etwfe_cluster_moments(inputs, est, X, yhat)
  } else {
    moments = list(
      Suu = crossprod(X, e2_sum * X),
      Sun = t(X * e_sum),
      Snn = diag(est[["n"]], nrow(est)),
      n_clusters = NULL
    )
  }
  meat = if (vcov_type_req == "cluster" || (vcov_type_req == "hc1" && !is.null(weights))) moments[["Suu"]]
  if (!is.null(meat)) attr(meat, "n_clusters") = moments[["n_clusters"]]
  vcov_mat = compute_vcov(
    vcov_type = vcov_type_req,
    strategy = "compress",
    XtX_inv = sol[["XtX_inv"]],
    rss = rss,
    df_res = df_res,
    nobs_orig = nobs_orig,
    n_params = ncol(X),
    X = X,
    rss_g = rss_g,
    meat = meat
  )

  att_kept = intersect(att_names, colnames(X))
  vcov_att = as.matrix(vcov_mat[att_kept, att_kept, drop = FALSE])
  attr(vcov_att, "type") = attr(vcov_mat, "type")
  attr(vcov_att, "n_clusters") = attr(vcov_mat, "n_clusters")
  attr(vcov_att, "rss") = rss
  attr(vcov_att, "tss") = tss
  moments[["bread"]] = sol[["XtX_inv"]]

  list(
    att_coef = setNames(as.numeric(betahat[att_kept, 1]), att_kept),
    att_vcov = vcov_att,
    moments = moments,
    cells = est,
    gref = design[["gref"]],
    tref = design[["tref"]],
    cgroup = cgroup,
    yvar = yvar,
    tvar = tvar_disp,
    gvar = gvar_disp,
    weights = weights,
    fe = NULL,
    collin.var = intersect(collin[["drop_names"]], att_names),
    query_string = query_string,
    nobs = nrow(est),
    nobs_orig = nobs_orig,
    df_residual = df_res,
    strategy = "compress"
  )
}

#' Clustered score and cell-count moments for dbetwfe()
#'
#' Computes, in SQL, the cross-products of the per-cluster scores (`u`) and
#' cell row counts (`n`) that the clustered (and unconditional) standard
#' errors need: `Suu` (the cluster meat), `Sun` and `Snn`. Each cluster's
#' residual sum in each cell (`e`) is formed from the cells' fitted values,
#' then summed over every pair of cells that share a cluster, so only one row
#' per pair of cells (or per cohort) is returned rather than one per cluster
#' and cell. Then `Suu = X' E X` and `Sun = X' F`, where `E` and `F` hold the
#' pairwise sums of `e * e` and `e * n`.
#'
#' When each cluster lies within one cohort (e.g. clustering by unit), the
#' pairs are formed by pivoting each cluster's periods into columns straight
#' from the rows, with one group per cluster; otherwise by a self-join on the
#' cluster.
#' @param est Estimation cells (rows aligned with `X` and `yhat`).
#' @keywords internal
etwfe_cluster_moments = function(inputs, est, X, yhat) {
  env2env(inputs, environment(), c("conn", "yvar", "tvar", "gvar", "weights", "cluster_var", "from_statement"))
  num = function(x) sprintf("%.17g", x)
  # Never-treated cells may have a missing cohort; match them by their recoded value
  na_code = est[[".g"]][is.na(est[[gvar]])][1]
  g_sql = if (is.na(na_code)) gvar else sprintf("COALESCE(%s, %s)", gvar, num(na_code))
  fitted_sql = paste(
    sprintf(
      "SELECT %d AS dbreg_cell, %s AS dbreg_fg, %s AS dbreg_ft, %s AS dbreg_yhat",
      seq_len(nrow(est)), num(est[[".g"]]), num(est[[".t"]]), num(yhat)
    ),
    collapse = " UNION ALL\n"
  )
  weights_expr = sql_weight_expr(weights)
  periods = sort(unique(est[[".t"]]))
  sql = list(
    fitted = fitted_sql, g = g_sql, t = tvar, y = yvar, cluster = cluster_var,
    weights = weights_expr, from = from_statement
  )
  pairs = if (length(periods) <= 50) etwfe_pairs_pivot(conn, sql, est, periods)
  if (is.null(pairs)) {
    pairs = etwfe_pairs_join(conn, sql)
  }
  pair = function(v, v_rev = v) {
    m = matrix(0, nrow(est), nrow(est))
    m[cbind(pairs[["c1"]], pairs[["c2"]])] = pairs[[v]]
    m[cbind(pairs[["c2"]], pairs[["c1"]])] = pairs[[v_rev]]
    m
  }
  list(
    Suu = crossprod(X, pair("ee") %*% X),
    Sun = crossprod(X, pair("en", "ne")),
    Snn = pair("nn"),
    n_clusters = attr(pairs, "n_clusters")
  )
}

# Pairwise cluster sums via a self-join on the cluster: each cluster's
# residual sum (e) and row count (n) per cell, then every pair of cells (once,
# c1 <= c2) that share a cluster
etwfe_pairs_join = function(conn, sql) {
  res = dbGetQuery(conn, paste0(
    "WITH fitted AS (\n", sql[["fitted"]], "\n),\n",
    "cells AS (\n",
    "  SELECT ", sql[["cluster"]], " AS dbreg_cl, ", sql[["g"]], " AS dbreg_g, ", sql[["t"]], " AS dbreg_t,\n",
    "    COUNT(*) AS n, ",
    if (is.null(sql[["weights"]])) "COUNT(*)" else paste0("SUM(", sql[["weights"]], ")"), " AS w, ",
    sql_weighted_sum(sql[["y"]], sql[["weights"]], "a"), "\n",
    "  ", sql[["from"]], "\n",
    "  GROUP BY ", sql[["cluster"]], ", ", sql[["g"]], ", ", sql[["t"]], "\n",
    "),\n",
    "resid AS (\n",
    "  SELECT c.dbreg_cl, f.dbreg_cell AS cell, 1.0 * c.n AS n, c.a - c.w * f.dbreg_yhat AS e\n",
    "  FROM cells c JOIN fitted f ON c.dbreg_g = f.dbreg_fg AND c.dbreg_t = f.dbreg_ft\n",
    ")\n",
    "SELECT l.cell AS c1, r.cell AS c2, SUM(l.e * r.e) AS ee, SUM(l.e * r.n) AS en, SUM(l.n * r.e) AS ne, SUM(l.n * r.n) AS nn,\n",
    "  (SELECT COUNT(DISTINCT dbreg_cl) FROM resid) AS n_clusters\n",
    "FROM resid l JOIN resid r ON l.dbreg_cl = r.dbreg_cl AND l.cell <= r.cell\n",
    "GROUP BY l.cell, r.cell"
  ))
  structure(res, n_clusters = res[["n_clusters"]][1])
}

# Pairwise cluster sums when every cluster lies within one cohort: subtract
# each row's fitted value, pivot each cluster's periods into columns (one
# group per cluster, in a single scan), then sum products by cohort. Returns
# NULL if some cluster spans cohorts.
etwfe_pairs_pivot = function(conn, sql, est, periods) {
  num = function(x) sprintf("%.17g", x)
  k = seq_along(periods)
  e_row = if (is.null(sql[["weights"]])) {
    sprintf("(%s - f.dbreg_yhat)", sql[["y"]])
  } else {
    sprintf("(%s) * (%s - f.dbreg_yhat)", sql[["weights"]], sql[["y"]])
  }
  pivot_cols = c(
    sprintf("SUM(CASE WHEN dbreg_t = %s THEN e END) AS e%d", num(periods), k),
    sprintf("SUM(CASE WHEN dbreg_t = %s THEN 1.0 END) AS n%d", num(periods), k)
  )
  ij = which(upper.tri(diag(length(k)), diag = TRUE), arr.ind = TRUE)
  i = ij[, 1]
  j = ij[, 2]
  sum_cols = c(
    sprintf("SUM(e%d * e%d) AS ee_%d_%d", i, j, i, j),
    sprintf("SUM(e%d * n%d) AS en_%d_%d", i, j, i, j),
    sprintf("SUM(n%d * e%d) AS ne_%d_%d", i, j, i, j),
    sprintf("SUM(n%d * n%d) AS nn_%d_%d", i, j, i, j)
  )
  res = dbGetQuery(conn, paste0(
    "WITH fitted AS (\n", sql[["fitted"]], "\n),\n",
    "obs AS (\n",
    "  SELECT ", sql[["cluster"]], " AS dbreg_cl, f.dbreg_fg AS dbreg_g, f.dbreg_ft AS dbreg_t, ", e_row, " AS e\n",
    "  FROM (SELECT * ", sql[["from"]], ") src\n",
    "  JOIN fitted f ON ", sql[["g"]], " = f.dbreg_fg AND ", sql[["t"]], " = f.dbreg_ft\n",
    "),\n",
    "pivoted AS (\n  SELECT dbreg_cl, dbreg_g, ", paste(pivot_cols, collapse = ", "), "\n",
    "  FROM obs GROUP BY dbreg_cl, dbreg_g\n)\n",
    "SELECT dbreg_g, COUNT(*) AS n_clusters, ", paste(sum_cols, collapse = ", "), ",\n",
    "  (SELECT COUNT(*) FROM (SELECT dbreg_cl FROM pivoted GROUP BY dbreg_cl HAVING COUNT(*) > 1) split) AS n_split\n",
    "FROM pivoted GROUP BY dbreg_g"
  ))
  if (any(res[["n_split"]] > 0)) {
    return(NULL)
  }
  # Cohort-by-period sums -> cell pairs
  cell = setNames(seq_len(nrow(est)), paste(est[[".g"]], est[[".t"]]))
  out = do.call(rbind, lapply(seq_len(nrow(res)), function(r) {
    c1 = cell[paste(res[["dbreg_g"]][r], periods[i])]
    c2 = cell[paste(res[["dbreg_g"]][r], periods[j])]
    ok = !is.na(c1) & !is.na(c2)
    get = function(v) unlist(res[r, sprintf("%s_%d_%d", v, i, j)], use.names = FALSE)[ok]
    data.frame(c1 = c1[ok], c2 = c2[ok], ee = get("ee"), en = get("en"), ne = get("ne"), nn = get("nn"))
  }))
  out[is.na(out)] = 0
  structure(out, n_clusters = sum(res[["n_clusters"]]))
}

#' Aggregate ETWFE treatment effects
#'
#' @md
#' @description
#' Aggregates the cohort-by-period treatment effects of a [dbetwfe()] model,
#' as `etwfe::emfx()` does. Each aggregate is an average of cell effects,
#' weighted by the number of treated observations in each cell; standard
#' errors follow exactly from the model's variance-covariance matrix. No data
#' are queried, so switching between aggregations is instant.
#'
#' @param x A [dbetwfe()] model.
#' @param aggregate One of `"event"`, `"simple"`, `"group"`, `"calendar"` or
#'   `"none"`. See [dbetwfe()].
#' @param post_only,window See [dbetwfe()].
#' @param unconditional Logical. Report "unconditional" standard errors, which
#'   also account for sampling variation in the cells' shares of each
#'   aggregate (the `etwfe::emfx()` default, via
#'   `marginaleffects::vcovUnconditional()`)? They use heteroskedasticity-robust
#'   (HC0) scores, clustered if the model is clustered. If `FALSE`, standard
#'   errors condition on the shares and follow from the model's
#'   variance-covariance matrix (`emfx(..., vcov = TRUE)`). Ignored for
#'   `aggregate = "none"`.
#' @param ... Currently ignored.
#' @return A `dbetwfe` object with the requested aggregation.
#' @export
#' @examples
#' if (requireNamespace("fixest", quietly = TRUE)) {
#'   data("base_stagg", package = "fixest")
#'   es = dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg)
#'   aggregate(es, "simple")
#'   aggregate(es, "event", window = 3)
#' }
aggregate.dbetwfe = function(
  x,
  aggregate = c("event", "simple", "group", "calendar", "none"),
  post_only = TRUE,
  window = NULL,
  unconditional = TRUE,
  ...
) {
  aggregate = match.arg(aggregate)
  b = x[["att_coef"]]
  V = x[["att_vcov"]]
  if (aggregate == "none") {
    W = diag(length(b))
    dimnames(W) = list(names(b), names(b))
    V_agg = V
  } else {
    S = etwfe_shares(x, aggregate, post_only, window)
    # Cell effects (zero for cells without their own effect)
    cell_att = outer(x[["cells"]][[".att"]], names(b), function(a, nm) as.numeric(!is.na(a) & a == nm))
    W = S %*% cell_att
    colnames(W) = names(b)
    V_agg = if (isTRUE(unconditional)) etwfe_unconditional_vcov(x, S, W, drop(cell_att %*% b)) else W %*% V %*% t(W)
  }
  est = drop(W %*% b)
  # Aggregates of only reference (zero) effects have no standard error
  no_se = rowSums(W != 0) == 0
  V_agg[no_se, ] = NA
  V_agg[, no_se] = NA
  for (a in c("type", "n_clusters", "rss", "tss")) attr(V_agg, a) = attr(V, a)
  if (aggregate != "none" && isTRUE(unconditional)) {
    attr(V_agg, "type") = "unconditional"
    attr(V_agg, "n_clusters") = x[["moments"]][["n_clusters"]]
  }
  dimnames(V_agg) = list(rownames(W), rownames(W))
  x[["coeftable"]] = gen_coeftable(matrix(est, dimnames = list(rownames(W), NULL)), V_agg, x[["df_residual"]])
  x[["vcov"]] = V_agg
  x[["aggregate"]] = aggregate
  x
}

#' Unconditional variance of ETWFE aggregates
#'
#' As in `marginaleffects::vcovUnconditional()` (the `etwfe::emfx()` default),
#' accounts for sampling variation in the cells' shares as well as in the
#' coefficients: each aggregate's influence combines the coefficients'
#' (HC0-type) influence with each observation's deviation of its cell effect
#' from the aggregate, summed by cluster if the model is clustered.
#' @param S Aggregate-by-cell share matrix from [etwfe_shares()].
#' @param W Aggregation weights on the treatment effects.
#' @param theta_cell Effect of each cell.
#' @keywords internal
etwfe_unconditional_vcov = function(x, S, W, theta_cell) {
  m = x[["moments"]]
  # Coefficient influence, mapped through the aggregation weights
  A = matrix(0, nrow(W), ncol(m[["bread"]]), dimnames = list(rownames(W), colnames(m[["bread"]])))
  A[, colnames(W)] = W
  G = A %*% m[["bread"]]
  # Share influence: (cell effect - aggregate) / N for each observation used
  theta = drop(W %*% x[["att_coef"]])
  D = sweep(S, 2, x[["cells"]][["n"]], "/") * outer(theta, theta_cell, function(a, b) b - a)
  V = G %*% m[["Suu"]] %*% t(G) + G %*% m[["Sun"]] %*% t(D) + D %*% t(m[["Sun"]]) %*% t(G) + D %*% m[["Snn"]] %*% t(D)
  if (!is.null(m[["n_clusters"]])) {
    V = V * m[["n_clusters"]] / (m[["n_clusters"]] - 1)
  }
  V
}

#' Aggregation shares for ETWFE effects (as in etwfe::emfx())
#'
#' One row per aggregate and one column per cell: each cell's share of the
#' aggregate's observations. Cells without their own effect (e.g. each
#' cohort's reference period) count as zero effects, as in `emfx()`.
#' @keywords internal
etwfe_shares = function(x, aggregate, post_only = TRUE, window = NULL) {
  cells = x[["cells"]]
  g = cells[[".g"]]
  t = cells[[".t"]]
  dtreat = cells[[".dtreat"]]
  # Observations averaged over, following emfx()
  if (x[["cgroup"]] == "never") {
    use = g != x[["gref"]]
    if (aggregate != "event") use = use & dtreat & t >= g
  } else if (aggregate == "event" && !isTRUE(post_only)) {
    use = g != x[["gref"]]
  } else {
    use = dtreat & !is.na(cells[[".att"]])
  }
  event = t - g
  if (!is.null(window)) {
    if (!length(window) %in% 1:2) stop("`window` must be of length 1 or 2.", call. = FALSE)
    window = rep_len(window, 2)
    use = use & event >= -window[1] & event <= window[2]
  }
  key = switch(aggregate, simple = rep(0, length(t)), group = g, calendar = t, event = event)
  levels = sort(unique(key[use]))
  label = switch(
    aggregate,
    simple = "ATT",
    group = paste0(x[["gvar"]], "::", levels),
    calendar = paste0(x[["tvar"]], "::", levels),
    event = paste0("event::", levels)
  )
  S = outer(levels, key, function(l, k) as.numeric(k == l)) * rep(use * cells[["n"]], each = length(levels))
  S = S / rowSums(S)
  dimnames(S) = list(label, NULL)
  S
}

#' @export
print.dbetwfe = function(x, ...) {
  ct = x[["coeftable"]]
  colnames(ct) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  agg = switch(
    x[["aggregate"]],
    event = "by event time",
    simple = "overall (simple ATT)",
    group = "by cohort",
    calendar = "by period",
    none = "none (cohort-by-period effects)"
  )
  cat("Extended TWFE (ETWFE) estimation, Dep. Var.:", x[["yvar"]], "\n")
  cat(
    "Observations.:", prettyNum(x[["nobs_orig"]], big.mark = ","), "(original) |",
    prettyNum(x[["nobs"]], big.mark = ","), "(cohort-by-period cells)\n"
  )
  cat("Control group:", if (x[["cgroup"]] == "never") "never treated" else "not-yet treated", "\n")
  cat("Aggregation:", agg, "\n")
  cat("Standard Errors:", format_vcov_label(attr(x[["vcov"]], "type"), attr(x[["vcov"]], "n_clusters")), "\n")
  print_coeftable(ct, gof_vals = gof(x), has_fes = TRUE)
  invisible(ct)
}

#' Plot ETWFE treatment effects
#'
#' @md
#' @description
#' Plots aggregated [dbetwfe()] treatment effects with confidence intervals,
#' e.g. an event-study plot for `aggregate = "event"`.
#' @param x A [dbetwfe()] model.
#' @param level Confidence level.
#' @param ... Additional arguments passed to [tinyplot::tinyplot()].
#' @return No return value, called for its side effect of producing a plot.
#' @export
plot.dbetwfe = function(x, level = 0.95, ...) {
  if (x[["aggregate"]] %in% c("simple", "none")) {
    stop("Plotting needs aggregate = \"event\", \"group\" or \"calendar\".", call. = FALSE)
  }
  ct = x[["coeftable"]]
  pos = as.numeric(sub(".*::", "", rownames(ct)))
  crit = stats::qt(1 - (1 - level) / 2, x[["df_residual"]])
  se = ifelse(is.na(ct[, "std.error"]), 0, ct[, "std.error"])
  xlab = switch(x[["aggregate"]], event = "Time since treatment", group = x[["gvar"]], calendar = x[["tvar"]])
  tinyplot::tinyplot(
    x = pos,
    y = ct[, "estimate"],
    ymin = ct[, "estimate"] - crit * se,
    ymax = ct[, "estimate"] + crit * se,
    type = "pointrange",
    xlab = xlab,
    ylab = "Estimate",
    ...
  )
  graphics::abline(h = 0, lty = 2)
  invisible(x)
}

#' @export
predict.dbetwfe = function(object, ...) {
  stop("predict() is not supported for dbetwfe models.", call. = FALSE)
}
