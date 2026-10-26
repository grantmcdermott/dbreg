# Strategy execution functions for dbreg
# Each function takes an inputs environment and returns a result list.

#' Execute moments strategy (no fixed effects)
#' @keywords internal
execute_moments_strategy = function(inputs) {
  env2env(
    inputs,
    environment(), c(
      "from_statement", "conn", "sql_only", "verbose",
      "data_only", "vcov_type_req", "fml", "fe", "intercept", "xvars", "yvar",
      "weights", "compression_ratio_est"
    )
  )
  # Get SQL expressions for design matrix terms
  # For interactions/factors, this expands to CASE WHEN expressions
  # Expand factors and interactions to SQL expressions (numeric terms pass
  # through unchanged)
  table_ref = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
  sql_design = sql_model_matrix(
    fml,
    conn,
    table_ref,
    expand = "all",
    fe_vars = fe,
    intercept = intercept
  )
  xvars_sql = sql_design[["select_exprs"]]
  xvar_names = sql_design[["aliases"]]
  register_factor_display(inputs, sql_design[["factor_levels"]], sql_design[["level_aliases"]], sql_design[["i_columns"]])
  
  frame = build_base_frame(inputs, xvars_sql, xvar_names)
  moments_sql = frame_moments_sql(frame, yvar, xvar_names, conn)

  if (sql_only) {
    return(moments_sql)
  }
  if (verbose) {
    message(if (!is.null(weights)) "[dbreg] Executing weighted moments SQL\n" else "[dbreg] Executing moments SQL\n")
  }
  moments_df = dbGetQuery(conn, moments_sql)
  if (data_only) {
    return(moments_df)
  }
  n_total = moments_df[["n_total"]]

  S = cross_moments(moments_df, xvar_names, yvar, intercept = frame[["intercept"]])
  vars_all = c(if (frame[["intercept"]]) "(Intercept)", xvar_names)
  p = length(vars_all)
  fit = fit_ols(S, yvar, vars_all, drop_collinear = TRUE, verbose = verbose)
  betahat = fit[["coef"]]
  p = length(fit[["x"]])
  rss = fit[["rss"]]
  tss = moment_tss(S, yvar)
  df_res = max(n_total - p, 1)
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = fit_meat(fit, frame, yvar, inputs)
  
  vcov_mat = compute_vcov(
    vcov_type = vcov_type_req,
    strategy = "moments",
    XtX_inv = fit[["bread"]],
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    n_params = p,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = fml,
    yvar = yvar,
    xvars = standardize_coef_names(xvars),
    fe = NULL,
    weights = weights,
    query_string = moments_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "moments",
    compression_ratio_est = compression_ratio_est,
    df_residual = df_res
  )
}

#' Execute demean strategy (1+ fixed effects)
#' 
#' Double demeaning / within estimator. Gives identical coefficients to 
#' fixed effects regression.
#' 
#' @keywords internal
execute_demean_strategy = function(inputs) {
  env2env(
    inputs,
    environment(),
    c(
      "from_statement", "conn", "sql_only", "data_only",
      "vcov_type_req", "fml", "fe", "xvars", "yvar", "verbose", "weights",
      "compression_ratio_est", "ssc"
    )
  )
  # Expand factors and interactions to SQL expressions (numeric terms pass
  # through unchanged)
  table_ref = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
  sql_design = sql_model_matrix(
    fml,
    conn,
    table_ref,
    expand = "all",
    fe_vars = fe
  )
  xvars_sql = sql_design[["select_exprs"]]
  xvar_names = sql_design[["aliases"]]
  register_factor_display(inputs, sql_design[["factor_levels"]], sql_design[["level_aliases"]], sql_design[["i_columns"]])
  
  frame = build_demean_frame(inputs, xvars_sql, xvar_names)
  on.exit(frame[["cleanup"]](), add = TRUE)
  demean_sql = frame_moments_sql(frame, yvar, xvar_names, conn)
  if (sql_only) {
    return(demean_sql)
  }

  # Execute SQL and build matrices
  if (verbose) {
    message(if (!is.null(weights)) "[dbreg] Executing weighted demean SQL\n" else "[dbreg] Executing demean SQL\n")
  }
  demean_df = dbGetQuery(conn, demean_sql)
  if (data_only) {
    return(demean_df)
  }
  n_total = demean_df[["n_total"]]
  n_fe_levels = vapply(seq_along(fe), function(k) {
    val = demean_df[[sprintf("n_fe%d", k)]]
    if (is.null(val)) 1L else as.integer(val)
  }, integer(1))

  S = cross_moments(demean_df, xvar_names, yvar, intercept = FALSE)
  fit = fit_ols(S, yvar, xvar_names, drop_collinear = TRUE, verbose = verbose)
  betahat = fit[["coef"]]
  xvar_names_kept = fit[["x"]]
  collin_vars = fit[["dropped"]]
  p_kept = length(xvar_names_kept)
  rss = fit[["rss"]]
  n_fe = length(fe)
  df_fe = sum(n_fe_levels) - (n_fe - 1)
  df_res = max(n_total - p_kept - df_fe, 1)
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = fit_meat(fit, frame, yvar, inputs)
  n_params_cluster = p_kept + df_fe  # K for CR1 correction
  if (vcov_type_req == "cluster") {
    # For ssc = "nested", exclude nested FE levels from K
    if (ssc == "nested") {
      n_params_cluster = p_kept + df_fe - nested_fe_df(inputs)
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = vcov_type_req,
    strategy = "demean",
    XtX_inv = fit[["bread"]],
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    n_params = n_params_cluster,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = moment_tss(S, yvar)

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = fml,
    yvar = yvar,
    xvars = standardize_coef_names(xvar_names_kept),
    collin.var = standardize_coef_names(collin_vars),
    fe = fe,
    weights = weights,
    query_string = demean_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "demean",
    compression_ratio_est = compression_ratio_est,
    df_residual = df_res,
    n_fe_levels = n_fe_levels
  )
}

#' Execute true Mundlak/CRE strategy
#'
#' Regresses Y on X plus group means of X for each fixed effect.
#' Y is NOT demeaned - predictions are on the original scale.
#'
#' @keywords internal
execute_mundlak_strategy = function(inputs) {
  env2env(
    inputs,
    environment(), c(
      "from_statement", "conn", "cluster_var", "sql_only",
      "verbose", "data_only", "vcov_type_req", "yvar", "fe", "fml", "xvars",
      "weights", "compression_ratio_est"
    )
  )
  n_fe = length(fe)

  if (n_fe == 0) {
    stop("mundlak strategy requires at least one fixed effect")
  }

  # Expand factors and interactions to SQL expressions (numeric terms pass
  # through unchanged)
  table_ref = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
  sql_design = sql_model_matrix(
    fml,
    conn,
    table_ref,
    expand = "all",
    fe_vars = fe
  )
  xvars_sql = sql_design[["select_exprs"]]
  xvar_names = sql_design[["aliases"]]
  register_factor_display(inputs, sql_design[["factor_levels"]], sql_design[["level_aliases"]], sql_design[["i_columns"]])
  
  weights_expr_base = sql_weight_expr(weights)
  weights_expr_aug = if (is.null(weights)) NULL else sql_weight_expr("weights")


  # Build base CTE with expanded columns AND original xvars (for group means)
  base_select = c(fe, yvar, xvars)
  for (i in seq_along(xvar_names)) {
    # Only add expanded terms that aren't already in original xvars
    if (!xvar_names[i] %in% xvars) {
      base_select = c(base_select, sprintf("%s AS %s", xvars_sql[i], xvar_names[i]))
    }
  }
  if (!is.null(weights) && !weights %in% c(fe, yvar, xvars, xvar_names)) {
    base_select = c(base_select, weights)
  }
  if (!is.null(cluster_var) && !cluster_var %in% c(fe, yvar, xvars, xvar_names, weights)) {
    base_select = c(base_select, cluster_var)
  }

  # Group means of every regressor column (incl. factor dummies and
  # interactions), one set per FE. Anything less isn't FE-equivalent (#77).
  # Means get opaque internal names, with display names registered below.
  cte_parts = character(0)
  join_parts = character(0)
  aug_bar_parts = character(0)
  xbar_all = character(0)
  for (k in seq_along(fe)) {
    fe_k = fe[k]
    xbar_k = if (length(xvar_names)) sprintf("dbreg_bar%d_%d", k, seq_along(xvar_names)) else character(0)
    xbar_all = c(xbar_all, xbar_k)
    means_cols = vapply(
      seq_along(xvar_names),
      function(i) sql_weighted_mean(xvar_names[i], weights_expr_base, xbar_k[i]),
      character(1)
    )
    cte_parts = c(cte_parts, if (length(means_cols)) {
      sprintf("fe%d_means AS (SELECT %s, %s FROM base GROUP BY %s)", k, fe_k, paste(means_cols, collapse = ", "), fe_k)
    } else {
      sprintf("fe%d_means AS (SELECT DISTINCT %s FROM base)", k, fe_k)
    })
    join_parts = c(join_parts, sprintf("JOIN fe%d_means m%d ON b.%s = m%d.%s", k, k, fe_k, k, fe_k))
    aug_bar_parts = c(aug_bar_parts, paste0("m", k, ".", xbar_k))
  }

  # Select columns for augmented table (include FE for counting)
  aug_select_parts = c(sprintf("b.%s", fe), sprintf("b.%s", yvar), sprintf("b.%s", xvar_names))
  if (!is.null(cluster_var) && !cluster_var %in% c(fe, yvar, xvar_names)) {
    aug_select_parts = c(aug_select_parts, sprintf("b.%s AS %s", cluster_var, cluster_var))
  }
  if (!is.null(weights)) {
    aug_select_parts = c(aug_select_parts, sprintf("b.%s AS weights", weights))
  }
  aug_select = paste(c(aug_select_parts, aug_bar_parts), collapse = ", ")

  # Display names for the means: "<coef name>_bar_<fe>"
  map = inputs[["name_map"]]
  coef_map = map
  is_var = names(coef_map) %in% names(inputs[["source_map"]])
  coef_map[is_var] = coef_var_name(coef_map[is_var])
  term_display = standardize_coef_names(restore_names(xvar_names, coef_map))
  fe_display = restore_names(fe, map)
  mundlak_means = data.frame(
    term = rep(term_display, length(fe)),
    fe = rep(fe_display, each = length(xvar_names)),
    name = paste0(rep(term_display, length(fe)), "_bar_", rep(fe_display, each = length(xvar_names)))
  )
  inputs[["name_map"]] = c(map, setNames(mundlak_means[["name"]], xbar_all))

  # Regressors before means, so collinearity (e.g. a regressor that's constant
  # within groups) drops the mean and keeps the regressor, as in CRE
  all_regressors = c(xvar_names, xbar_all)

  frame = list(
    cte_sql = paste0(
      "WITH base AS (SELECT ", paste(base_select, collapse = ", "), " ", from_statement, "),\n",
      paste(cte_parts, collapse = ",\n"), ",\n",
      "augmented AS (SELECT ", aug_select, " FROM base b ", paste(join_parts, collapse = " "), ")"
    ),
    cte_name = "augmented",
    cols = setNames(c(yvar, all_regressors), c(yvar, all_regressors)),
    weights_expr = weights_expr_aug,
    intercept = TRUE,
    cast = TRUE,
    prefix_terms = c(
      sql_count(conn, "n_total"),
      if (n_fe >= 1) sql_count(conn, "n_fe1", fe[1], distinct = TRUE) else "1 AS n_fe1",
      if (n_fe >= 2) sql_count(conn, "n_fe2", fe[2], distinct = TRUE) else "1 AS n_fe2"
    )
  )
  mundlak_sql = frame_moments_sql(frame, yvar, all_regressors, conn)

  if (sql_only) {
    return(mundlak_sql)
  }

  if (verbose) {
    message(if (!is.null(weights)) "[dbreg] Executing weighted mundlak SQL\n" else "[dbreg] Executing mundlak SQL\n")
  }
  mundlak_df = dbGetQuery(conn, mundlak_sql)
  if (data_only) {
    return(mundlak_df)
  }

  n_total = mundlak_df[["n_total"]]
  n_fe1 = mundlak_df[["n_fe1"]]
  n_fe2 = mundlak_df[["n_fe2"]]

  S = cross_moments(mundlak_df, all_regressors, yvar)
  vars_all = c("(Intercept)", all_regressors)
  p = length(vars_all)
  fit = fit_ols(S, yvar, vars_all, drop_collinear = TRUE, verbose = verbose)
  betahat = fit[["coef"]]
  p = length(fit[["x"]])
  rss = fit[["rss"]]
  tss = moment_tss(S, yvar)
  df_res = max(n_total - p, 1)

  # Compute meat matrix if needed (HC1 or cluster)
  meat = fit_meat(fit, frame, yvar, inputs)

  vcov_mat = compute_vcov(
    vcov_type = vcov_type_req,
    strategy = "mundlak",
    XtX_inv = fit[["bread"]],
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    n_params = p,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = fml,
    yvar = yvar,
    xvars = standardize_coef_names(xvar_names),
    collin.var = fit[["dropped"]],
    fe = fe,
    weights = weights,
    query_string = mundlak_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "mundlak",
    compression_ratio_est = compression_ratio_est,
    df_residual = df_res,
    mundlak_means = mundlak_means,
    n_fe1 = n_fe1,
    n_fe2 = n_fe2
  )
}

#' Execute compress strategy (groupby compression)
#' @keywords internal
execute_compress_strategy = function(inputs) {
  env2env(
    inputs,
    environment(),
    c(
      "has_interactions", "conn", "sql_only", "data_only", "vcov_type_req",
      "fml", "fe", "xvars", "weights", "yvar", "verbose", "intercept",
      "compression_ratio_est", "ssc"
    )
  )
  from_statement = inputs[["from_statement"]]
  # catch for sampled (limited) queries
  if (grepl("LIMIT\\s+\\d+\\s*$", from_statement, ignore.case = TRUE)) {
    from_statement = glue("FROM (SELECT * {from_statement})")
  }

  # Handle interactions: expand to SQL expressions
  if (isTRUE(has_interactions)) {
    # Extract table name from FROM statement for sql_model_matrix
    table_ref = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
    
    # Get SQL expansions for RHS terms (expand interactions only, keep main effects as-is)
    sql_design = sql_model_matrix(
      fml,
      conn,
      table_ref,
      expand = "interactions",
      fe_vars = fe,
      intercept = intercept
    )
    
    # Build SELECT expressions with aliases
    select_exprs = paste0(sql_design[["select_exprs"]], " AS ", sql_design[["aliases"]])
    xvars_sql = paste(select_exprs, collapse = ", ")
    xvar_names = sql_design[["aliases"]]
    register_factor_display(inputs, sql_design[["factor_levels"]], sql_design[["level_aliases"]], sql_design[["i_columns"]])
  } else {
    xvars_sql = paste(xvars, collapse = ", ")
    xvar_names = xvars
  }
  
  weights_expr = sql_weight_expr(weights)
  # FE columns (no expansion needed - used for grouping)
  fe_sql = if (length(fe)) paste(fe, collapse = ", ") else NULL
  
  # Combined columns for SELECT and GROUP BY
  all_cols_sql = if (!is.null(fe_sql)) paste(xvars_sql, fe_sql, sep = ", ") else xvars_sql
  group_cols = if (!is.null(fe_sql)) c(xvar_names, fe) else xvar_names
  group_cols_sql = paste(group_cols, collapse = ", ")
  moment_terms = build_weighted_moment_terms(
    y_sql = yvar,
    weights_expr = weights_expr,
    prefix_terms = "COUNT(*) AS n",
    include_w_sq = TRUE
  )
  
  query_string = paste0(
    "WITH cte AS (\n    SELECT\n        ",
    all_cols_sql,
    ",\n        ",
    paste(moment_terms, collapse = ",\n        "),
    "\n    ",
    from_statement,
    "\n    GROUP BY ",
    group_cols_sql,
    "\n    )\n    SELECT\n    *,\n    sum_wy / sum_w AS mean_Y,\n    sqrt(sum_w) AS wts,\n",
    # Cell ids, reproducible in later queries (see compute_meat_cluster_compress_sql())
    "    ROW_NUMBER() OVER (ORDER BY ", group_cols_sql, ") AS dbreg_cell\n    FROM cte"
  )

  if (sql_only) {
    return(query_string)
  }
  if (verbose) {
    message(if (!is.null(weights)) "[dbreg] Executing weighted compress strategy SQL\n" else "[dbreg] Executing compress strategy SQL\n")
  }
  compressed_dat = dbGetQuery(conn, query_string)
  nobs_orig = sum(compressed_dat[["n"]])
  nobs_comp = nrow(compressed_dat)
  compression_ratio = nobs_comp / max(nobs_orig, 1)

  if (verbose && compression_ratio > 0.8) {
    warning(paste0(
      sprintf(
        "[dbreg] compression ineffective (%.1f%% of original rows). ",
        100 * compression_ratio
      ),
      sprintf("Consider strategy = '%s'.", if (length(fe)) "demean" else "moments")
    ))
  }

  if (length(fe)) {
    for (f in fe) {
      compressed_dat[[f]] = factor(compressed_dat[[f]])
    }
  }
  if (data_only) {
    compressed_dat[["dbreg_cell"]] = NULL
    return(compressed_dat)
  }

  # Build design matrix
  # Use expanded column names if interactions were present
  design_vars = if (isTRUE(has_interactions)) xvar_names else xvars
  # Factor dummies (incl. FE) built in R are named after their (safe) variable
  # and level; register display names for them
  is_cat = vapply(c(design_vars, fe), function(v) {
    v %in% names(compressed_dat) && (is.factor(compressed_dat[[v]]) || is.character(compressed_dat[[v]]))
  }, logical(1))
  register_factor_display(inputs, lapply(setNames(nm = c(design_vars, fe)[is_cat]), function(v) unique(compressed_dat[[v]])))
  X = sparse.model.matrix(
    reformulate(c(design_vars, fe), intercept = intercept),
    compressed_dat
  )
  if (ncol(X) == 0) {
    stop("Design matrix has zero columns.")
  }
  Y = compressed_dat[, "mean_Y"]
  wts = compressed_dat[["wts"]]
  Xw = X * wts
  Yw = Y * wts
  XtX = crossprod(Xw)
  XtY = crossprod(Xw, Yw)

  # Detect and handle collinearity
  collin = detect_collinearity(XtX, XtY, verbose = verbose)
  XtX = collin[["XtX"]]
  XtY = collin[["Xty"]]
  collin_vars = collin[["drop_names"]]
  if (collin[["collinear"]]) {
    keep_idx = match(collin[["keep_names"]], colnames(X))
    X = X[, keep_idx, drop = FALSE]
  }

  solve_result = solve_with_fallback(XtX, XtY)
  betahat = solve_result[["betahat"]]
  XtX_inv = solve_result[["XtX_inv"]]
  if (is.null(dim(betahat))) {
    betahat = matrix(betahat, ncol = 1)
  }
  rownames(betahat) = colnames(X)
  yhat = as.numeric(X %*% betahat)

  sum_w = compressed_dat[["sum_w"]]
  sum_wy = compressed_dat[["sum_wy"]]
  sum_wy_sq = compressed_dat[["sum_wy_sq"]]
  rss_g = sum_wy_sq - 2 * yhat * sum_wy + sum_w * (yhat^2)
  rss_total = sum(rss_g)
  df_res = max(nobs_orig - ncol(X), 1)

  # Calculate TSS for R2
  sum_wy_total = sum(compressed_dat[["sum_wy"]])
  sum_wy_sq_total = sum(compressed_dat[["sum_wy_sq"]])
  sum_w_total = sum(compressed_dat[["sum_w"]])
  tss = sum_wy_sq_total - (sum_wy_total^2 / sum_w_total)
  
  # For clustered SEs, need to query cluster-by-cell stats
  meat = NULL
  n_params_cluster = ncol(X)  # K for CR1 correction
  if (vcov_type_req == "hc1" && !is.null(weights)) {
    sum_w2 = compressed_dat[["sum_w2"]]
    sum_w2y = compressed_dat[["sum_w2y"]]
    sum_w2y_sq = compressed_dat[["sum_w2y_sq"]]
    rss_g_w2 = sum_w2y_sq - 2 * yhat * sum_w2y + sum_w2 * (yhat^2)
    meat = crossprod(X, Diagonal(x = as.numeric(rss_g_w2)) %*% X)
  }
  if (vcov_type_req == "cluster") {
    # Cells are keyed on the expanded columns, so expose them (and only the
    # other columns the query needs) to it
    cells_from = if (isTRUE(has_interactions)) {
      cols = setdiff(unique(c(inputs[["cluster_var"]], yvar, weights, fe)), xvar_names)
      paste0("FROM (SELECT ", paste(c(cols, xvars_sql), collapse = ", "), " ", from_statement, ") AS dbreg_cells")
    } else {
      from_statement
    }
    # Many clusters but few cells: sum the scores by cluster in the database
    # instead of fetching one row per cluster and cell
    meat = if (use_cluster_sql(nrow(compressed_dat), ncol(X), inputs)) {
      compute_meat_cluster_compress_sql(inputs, cells_from, group_cols, compressed_dat[["dbreg_cell"]], X, list(yhat))[[1]]
    } else {
      compute_meat_cluster_compress(inputs, cells_from, group_cols, compressed_dat, X, yhat)
    }
    # For ssc = "nested", exclude nested FE levels from K
    if (ssc == "nested") {
      n_params_cluster = ncol(X) - nested_fe_df(inputs, from_statement)
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = vcov_type_req,
    strategy = "compress",
    XtX_inv = XtX_inv,
    rss = rss_total,
    df_res = df_res,
    nobs_orig = nobs_orig,
    n_params = n_params_cluster,
    X = X,
    rss_g = rss_g,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss_total
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, max(nobs_orig - ncol(X), 1))

  # Coefficient names for xvars (excluding intercept and FE dummies)
  # Match coefficients that start with any design_var name
  all_coef_names = rownames(coeftable)
  all_coef_names = all_coef_names[all_coef_names != "(Intercept)"]
  design_pattern = paste0("^(", paste(standardize_coef_names(design_vars), collapse = "|"), ")")
  coef_names = all_coef_names[grepl(design_pattern, all_coef_names)]

  return(
    list(
      coeftable = coeftable,
      data = compressed_dat,
      vcov = vcov_mat,
      fml = fml,
      yvar = yvar,
      xvars = standardize_coef_names(xvars),
      collin.var = standardize_coef_names(collin_vars),
      coef_names = coef_names,
      fe = fe,
      weights = weights,
      query_string = query_string,
      nobs = nobs_comp,
      nobs_orig = nobs_orig,
      strategy = "compress",
      compression_ratio = compression_ratio,
      compression_ratio_est = compression_ratio_est,
      df_residual = max(nobs_orig - ncol(X), 1)
    )
  )
}

#' Degrees of freedom of FE variables nested within cluster variable
#' 
#' For ssc = "nested", we exclude an FE's parameters from K if the FE is nested
#' within the cluster variable (i.e., each FE value belongs to exactly one
#' cluster). Matching fixest, each nested FE contributes G - 1 parameters (its
#' levels less one for the absorbed intercept), so the result is
#' sum(G_j - 1) over nested FEs.
#' 
#' @keywords internal
nested_fe_df = function(inputs, from_statement = inputs[["from_statement"]]) {
  env2env(inputs, environment(), c("conn", "fe", "cluster_var"))
  if (is.null(fe) || length(fe) == 0 || is.null(cluster_var)) {
    return(0L)
  }
  backend = detect_backend(conn)[["name"]]
  nested_df = 0L
  for (f in fe) {
    # Nested iff no FE value spans more than one cluster
    nested_sql = sql_limit(
      glue(
        "SELECT {f} FROM (SELECT * {from_statement}) t ",
        "GROUP BY {f} HAVING COUNT(DISTINCT {cluster_var}) > 1"
      ),
      1,
      backend
    )
    count_sql = glue("SELECT COUNT(DISTINCT {f}) AS n FROM (SELECT * {from_statement}) t")
    n_levels = tryCatch(
      if (nrow(dbGetQuery(conn, nested_sql)) == 0) dbGetQuery(conn, count_sql)[["n"]] else 0L,
      error = function(e) {
        # Fall back to keeping the FE in K (larger, conservative SEs), but say so
        warning(
          "[dbreg] Could not check whether fixed effect '", f, "' is nested in '",
          cluster_var, "' (", conditionMessage(e), "). Treating it as not nested ",
          "for the ssc = \"nested\" correction.",
          call. = FALSE
        )
        0L
      }
    )
    nested_df = nested_df + max(as.integer(n_levels) - 1L, 0L)
  }
  nested_df
}
