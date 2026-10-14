# Strategy execution functions for dbreg
# Each function takes an inputs environment and returns a result list.

#' Execute moments strategy (no fixed effects)
#' @keywords internal
execute_moments_strategy = function(inputs) {
  # Get SQL expressions for design matrix terms
  # For interactions/factors, this expands to CASE WHEN expressions
  if (isTRUE(inputs[["has_interactions"]])) {
    table_ref = sub("^FROM\\s+", "", inputs[["from_statement"]], ignore.case = TRUE)
    sql_design = sql_model_matrix(
      inputs[["fml"]],
      inputs[["conn"]],
      table_ref,
      expand = "all",
      fe_vars = inputs[["fe"]]
    )
    xvars_sql = sql_design[["select_exprs"]]
    xvar_names = sql_design[["col_names"]]
  } else {
    xvars_sql = inputs[["xvars"]]
    xvar_names = inputs[["xvars"]]
  }
  
  yvar = inputs[["yvar"]]
  frame = build_base_frame(inputs, xvars_sql, xvar_names)
  moments_sql = frame_moments_sql(frame, yvar, xvar_names, inputs[["conn"]])

  if (inputs[["sql_only"]]) {
    return(moments_sql)
  }
  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbreg] Executing weighted moments SQL\n" else "[dbreg] Executing moments SQL\n")
  }
  moments_df = dbGetQuery(inputs[["conn"]], moments_sql)
  if (inputs[["data_only"]]) {
    return(moments_df)
  }
  n_total = moments_df[["n_total"]]

  S = cross_moments(moments_df, xvar_names, yvar)
  vars_all = c("(Intercept)", xvar_names)
  p = length(vars_all)
  fit = fit_ols(S, yvar, vars_all)
  betahat = fit[["coef"]]
  rss = fit[["rss"]]
  tss = moment_tss(S, yvar)
  df_res = max(n_total - p, 1)
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = fit_meat(fit, frame, yvar, inputs)
  
  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
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
    fml = inputs[["fml"]],
    yvar = inputs[["yvar"]],
    xvars = standardize_coef_names(inputs[["xvars"]]),
    fe = NULL,
    weights = inputs[["weights"]],
    query_string = moments_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "moments",
    compression_ratio_est = inputs[["compression_ratio_est"]],
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
  # Handle interactions: expand to SQL expressions

  if (isTRUE(inputs[["has_interactions"]])) {
    table_ref = sub("^FROM\\s+", "", inputs[["from_statement"]], ignore.case = TRUE)
    sql_design = sql_model_matrix(
      inputs[["fml"]],
      inputs[["conn"]],
      table_ref,
      expand = "all",
      fe_vars = inputs[["fe"]]
    )
    xvars_sql = sql_design[["select_exprs"]]
    xvar_names = sql_design[["col_names"]]
  } else {
    xvars_sql = inputs[["xvars"]]
    xvar_names = inputs[["xvars"]]
  }
  
  yvar = inputs[["yvar"]]
  frame = build_demean_frame(inputs, xvars_sql, xvar_names)
  on.exit(frame[["cleanup"]](), add = TRUE)
  demean_sql = frame_moments_sql(frame, yvar, xvar_names, inputs[["conn"]])
  if (inputs[["sql_only"]]) {
    return(demean_sql)
  }

  # Execute SQL and build matrices
  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbreg] Executing weighted demean SQL\n" else "[dbreg] Executing demean SQL\n")
  }
  demean_df = dbGetQuery(inputs[["conn"]], demean_sql)
  if (inputs[["data_only"]]) {
    return(demean_df)
  }
  n_total = demean_df[["n_total"]]
  n_fe_levels = vapply(seq_along(inputs[["fe"]]), function(k) {
    val = demean_df[[sprintf("n_fe%d", k)]]
    if (is.null(val)) 1L else as.integer(val)
  }, integer(1))

  S = cross_moments(demean_df, xvar_names, yvar, intercept = FALSE)
  fit = fit_ols(S, yvar, xvar_names, drop_collinear = TRUE, verbose = inputs[["verbose"]])
  betahat = fit[["coef"]]
  xvar_names_kept = fit[["x"]]
  collin_vars = fit[["dropped"]]
  p_kept = length(xvar_names_kept)
  rss = fit[["rss"]]
  n_fe = length(inputs[["fe"]])
  df_fe = sum(n_fe_levels) - (n_fe - 1)
  df_res = max(n_total - p_kept - df_fe, 1)
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = fit_meat(fit, frame, yvar, inputs)
  n_params_cluster = p_kept + df_fe  # K for CR1 correction
  if (inputs[["vcov_type_req"]] == "cluster") {
    # For ssc = "nested", exclude nested FE levels from K
    if (inputs[["ssc"]] == "nested") {
      n_params_cluster = p_kept + df_fe - nested_fe_df(
        inputs[["conn"]], inputs[["from_statement"]], inputs[["fe"]], inputs[["cluster_var"]]
      )
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
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
    fml = inputs[["fml"]],
    yvar = yvar,
    xvars = standardize_coef_names(xvar_names_kept),
    collin.var = standardize_coef_names(collin_vars),
    fe = inputs[["fe"]],
    weights = inputs[["weights"]],
    query_string = demean_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "demean",
    compression_ratio_est = inputs[["compression_ratio_est"]],
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
  yvar = inputs[["yvar"]]
  fe = inputs[["fe"]]
  n_fe = length(fe)

  if (n_fe == 0) {
    stop("mundlak strategy requires at least one fixed effect")
  }

  # Handle interactions: expand to SQL expressions
  if (isTRUE(inputs[["has_interactions"]])) {
    table_ref = sub("^FROM\\s+", "", inputs[["from_statement"]], ignore.case = TRUE)
    sql_design = sql_model_matrix(
      inputs[["fml"]],
      inputs[["conn"]],
      table_ref,
      expand = "all",
      fe_vars = inputs[["fe"]]
    )
    xvars_sql = sql_design[["select_exprs"]]
    xvar_names = sql_design[["col_names"]]
  } else {
    xvars_sql = inputs[["xvars"]]
    xvar_names = inputs[["xvars"]]
  }
  
  weights_expr_base = sql_weight_expr(inputs[["weights"]])
  weights_expr_aug = if (is.null(inputs[["weights"]])) NULL else sql_weight_expr("weights")

  cluster_var = inputs[["cluster_var"]]

  # Build base CTE with expanded columns AND original xvars (for group means)
  base_select = c(fe, yvar, inputs[["xvars"]])
  for (i in seq_along(xvar_names)) {
    # Only add expanded terms that aren't already in original xvars
    if (!xvar_names[i] %in% inputs[["xvars"]]) {
      base_select = c(base_select, sprintf("%s AS %s", xvars_sql[i], xvar_names[i]))
    }
  }
  if (!is.null(inputs[["weights"]]) && !inputs[["weights"]] %in% c(fe, yvar, inputs[["xvars"]], xvar_names)) {
    base_select = c(base_select, inputs[["weights"]])
  }
  if (!is.null(cluster_var) && !cluster_var %in% c(fe, yvar, inputs[["xvars"]], xvar_names, inputs[["weights"]])) {
    base_select = c(base_select, cluster_var)
  }

  # Build group means CTEs and join clauses for each FE
  cte_parts = character(0)
  join_parts = character(0)
  xbar_all = character(0)

  # Build group means CTEs using ORIGINAL numeric xvars only
  # (can't compute AVG on factors; their means are handled via expanded dummies)
  # This follows the Mundlak/CRE approach of controlling for correlation
  # between original covariates and the FE
  if (isTRUE(inputs[["has_interactions"]])) {
    # Filter to numeric vars only (factors are in sql_design$factor_levels)
    factor_vars = names(sql_design[["factor_levels"]])
    numeric_xvars = setdiff(inputs[["xvars"]], factor_vars)
  } else {
    numeric_xvars = inputs[["xvars"]]
  }
  
  for (k in seq_along(fe)) {
    fe_k = fe[k]
    suffix = paste0("_bar_", fe_k)
    
    if (length(numeric_xvars) > 0) {
      xbar_k = paste0(numeric_xvars, suffix)
      xbar_all = c(xbar_all, xbar_k)
      means_cols = paste(
        vapply(
          numeric_xvars,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, suffix)),
          character(1)
        ),
        collapse = ", "
      )
      cte_parts = c(cte_parts, sprintf(
        "fe%d_means AS (SELECT %s, %s FROM base GROUP BY %s)",
        k, fe_k, means_cols, fe_k
      ))
    } else {
      # No numeric vars - just select FE for joining
      cte_parts = c(cte_parts, sprintf(
        "fe%d_means AS (SELECT DISTINCT %s FROM base)",
        k, fe_k
      ))
    }
    join_parts = c(join_parts, sprintf(
      "JOIN fe%d_means m%d ON b.%s = m%d.%s",
      k, k, fe_k, k, fe_k
    ))
  }

  # Select columns for augmented table (include FE for counting)
  aug_select_parts = c(sprintf("b.%s", fe), sprintf("b.%s", yvar), sprintf("b.%s", xvar_names))
  if (!is.null(cluster_var) && !cluster_var %in% c(fe, yvar, xvar_names)) {
    aug_select_parts = c(aug_select_parts, sprintf("b.%s AS %s", cluster_var, cluster_var))
  }
  if (!is.null(inputs[["weights"]])) {
    aug_select_parts = c(aug_select_parts, sprintf("b.%s AS weights", inputs[["weights"]]))
  }
  for (k in seq_along(fe)) {
    if (length(numeric_xvars) > 0) {
      suffix = paste0("_bar_", fe[k])
      xbar_k = paste0(numeric_xvars, suffix)
      aug_select_parts = c(aug_select_parts, paste0("m", k, ".", xbar_k))
    }
  }
  aug_select = paste(aug_select_parts, collapse = ", ")

  # All regressors: expanded X plus group means of numeric xvars
  all_regressors = c(xvar_names, xbar_all)

  frame = list(
    cte_sql = paste0(
      "WITH base AS (SELECT ", paste(base_select, collapse = ", "), " ", inputs[["from_statement"]], "),\n",
      paste(cte_parts, collapse = ",\n"), ",\n",
      "augmented AS (SELECT ", aug_select, " FROM base b ", paste(join_parts, collapse = " "), ")"
    ),
    cte_name = "augmented",
    cols = setNames(c(yvar, all_regressors), c(yvar, all_regressors)),
    weights_expr = weights_expr_aug,
    intercept = TRUE,
    cast = TRUE,
    prefix_terms = c(
      sql_count(inputs[["conn"]], "n_total"),
      if (n_fe >= 1) sql_count(inputs[["conn"]], "n_fe1", fe[1], distinct = TRUE) else "1 AS n_fe1",
      if (n_fe >= 2) sql_count(inputs[["conn"]], "n_fe2", fe[2], distinct = TRUE) else "1 AS n_fe2"
    )
  )
  mundlak_sql = frame_moments_sql(frame, yvar, all_regressors, inputs[["conn"]])

  if (inputs[["sql_only"]]) {
    return(mundlak_sql)
  }

  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbreg] Executing weighted mundlak SQL\n" else "[dbreg] Executing mundlak SQL\n")
  }
  mundlak_df = dbGetQuery(inputs[["conn"]], mundlak_sql)
  if (inputs[["data_only"]]) {
    return(mundlak_df)
  }

  n_total = mundlak_df[["n_total"]]
  n_fe1 = mundlak_df[["n_fe1"]]
  n_fe2 = mundlak_df[["n_fe2"]]

  S = cross_moments(mundlak_df, all_regressors, yvar)
  vars_all = c("(Intercept)", all_regressors)
  p = length(vars_all)
  fit = fit_ols(S, yvar, vars_all)
  betahat = fit[["coef"]]
  rss = fit[["rss"]]
  tss = moment_tss(S, yvar)
  df_res = max(n_total - p, 1)

  # Compute meat matrix if needed (HC1 or cluster)
  meat = fit_meat(fit, frame, yvar, inputs)

  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
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
    fml = inputs[["fml"]],
    yvar = yvar,
    xvars = standardize_coef_names(xvar_names),
    fe = fe,
    weights = inputs[["weights"]],
    query_string = mundlak_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "mundlak",
    compression_ratio_est = inputs[["compression_ratio_est"]],
    df_residual = df_res,
    n_fe1 = n_fe1,
    n_fe2 = n_fe2
  )
}

#' Execute compress strategy (groupby compression)
#' @keywords internal
execute_compress_strategy = function(inputs) {
  from_statement = inputs[["from_statement"]]
  # catch for sampled (limited) queries
  if (grepl("LIMIT\\s+\\d+\\s*$", from_statement, ignore.case = TRUE)) {
    from_statement = glue("FROM (SELECT * {from_statement})")
  }

  # Handle interactions: expand to SQL expressions
  if (isTRUE(inputs[["has_interactions"]])) {
    # Extract table name from FROM statement for sql_model_matrix
    table_ref = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
    
    # Get SQL expansions for RHS terms (expand interactions only, keep main effects as-is)
    sql_design = sql_model_matrix(
      inputs[["fml"]],
      inputs[["conn"]],
      table_ref,
      expand = "interactions",
      fe_vars = inputs[["fe"]]
    )
    
    # Build SELECT expressions with aliases
    select_exprs = paste0(sql_design[["select_exprs"]], " AS ", sql_design[["col_names"]])
    xvars_sql = paste(select_exprs, collapse = ", ")
    xvar_names = sql_design[["col_names"]]
  } else {
    xvars_sql = paste(inputs[["xvars"]], collapse = ", ")
    xvar_names = inputs[["xvars"]]
  }
  
  weights_expr = sql_weight_expr(inputs[["weights"]])
  # FE columns (no expansion needed - used for grouping)
  fe_sql = if (length(inputs[["fe"]])) paste(inputs[["fe"]], collapse = ", ") else NULL
  
  # Combined columns for SELECT and GROUP BY
  all_cols_sql = if (!is.null(fe_sql)) paste(xvars_sql, fe_sql, sep = ", ") else xvars_sql
  group_cols = if (!is.null(fe_sql)) c(xvar_names, inputs[["fe"]]) else xvar_names
  group_cols_sql = paste(group_cols, collapse = ", ")
  moment_terms = build_weighted_moment_terms(
    y_sql = inputs[["yvar"]],
    weights_expr = weights_expr,
    prefix_terms = "COUNT(*) AS n",
    include_w_sq = TRUE
  )
  
  query_string = paste0(
    "WITH cte AS (\n    SELECT\n        ",
    all_cols_sql,
    ",\n        ",
    paste(moment_terms, collapse = ",\n        "),
    ",\n    ",
    from_statement,
    "\n    GROUP BY ",
    group_cols_sql,
    "\n    )\n    SELECT\n    *,\n    sum_wy / sum_w AS mean_Y,\n    sqrt(sum_w) AS wts\n    FROM cte"
  )

  if (inputs[["sql_only"]]) {
    return(query_string)
  }
  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbreg] Executing weighted compress strategy SQL\n" else "[dbreg] Executing compress strategy SQL\n")
  }
  compressed_dat = dbGetQuery(inputs[["conn"]], query_string)
  nobs_orig = sum(compressed_dat[["n"]])
  nobs_comp = nrow(compressed_dat)
  compression_ratio = nobs_comp / max(nobs_orig, 1)

  if (inputs[["verbose"]] && compression_ratio > 0.8) {
    warning(paste0(
      sprintf(
        "[dbreg] compression ineffective (%.1f%% of original rows). ",
        100 * compression_ratio
      ),
      sprintf("Consider strategy = '%s'.", if (length(inputs[["fe"]])) "demean" else "moments")
    ))
  }

  if (length(inputs[["fe"]])) {
    for (f in inputs[["fe"]]) {
      compressed_dat[[f]] = factor(compressed_dat[[f]])
    }
  }
  if (inputs[["data_only"]]) {
    return(compressed_dat)
  }

  # Build design matrix
  # Use expanded column names if interactions were present
  design_vars = if (isTRUE(inputs[["has_interactions"]])) xvar_names else inputs[["xvars"]]
  X = sparse.model.matrix(
    reformulate(c(design_vars, inputs[["fe"]])),
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
  collin = detect_collinearity(XtX, XtY, verbose = inputs[["verbose"]])
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
  if (inputs[["vcov_type_req"]] == "hc1" && !is.null(inputs[["weights"]])) {
    sum_w2 = compressed_dat[["sum_w2"]]
    sum_w2y = compressed_dat[["sum_w2y"]]
    sum_w2y_sq = compressed_dat[["sum_w2y_sq"]]
    rss_g_w2 = sum_w2y_sq - 2 * yhat * sum_w2y + sum_w2 * (yhat^2)
    meat = crossprod(X, Diagonal(x = as.numeric(rss_g_w2)) %*% X)
  }
  if (inputs[["vcov_type_req"]] == "cluster") {
    meat = compute_meat_cluster_compress(
      conn = inputs[["conn"]],
      from_statement = from_statement,
      group_cols = group_cols,
      yvar = inputs[["yvar"]],
      cluster_var = inputs[["cluster_var"]],
      compressed_dat = compressed_dat,
      X = X,
      yhat = yhat,
      weights = inputs[["weights"]]
    )
    # For ssc = "nested", exclude nested FE levels from K
    if (inputs[["ssc"]] == "nested") {
      n_params_cluster = ncol(X) - nested_fe_df(
        inputs[["conn"]], from_statement, inputs[["fe"]], inputs[["cluster_var"]]
      )
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
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
      fml = inputs[["fml"]],
      yvar = inputs[["yvar"]],
      xvars = standardize_coef_names(inputs[["xvars"]]),
      collin.var = standardize_coef_names(collin_vars),
      coef_names = coef_names,
      fe = inputs[["fe"]],
      weights = inputs[["weights"]],
      query_string = query_string,
      nobs = nobs_comp,
      nobs_orig = nobs_orig,
      strategy = "compress",
      compression_ratio = compression_ratio,
      compression_ratio_est = inputs[["compression_ratio_est"]],
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
nested_fe_df = function(conn, from_statement, fe, cluster_var) {
  if (is.null(fe) || length(fe) == 0 || is.null(cluster_var)) {
    return(0L)
  }
  
  nested_df = 0L
  for (f in fe) {
    # Check if FE is nested: each FE value should map to exactly one cluster
    # If any FE value spans multiple clusters, it's not nested
    nested_sql = glue(
      "SELECT 1 FROM (SELECT * {from_statement}) t ",
      "GROUP BY {f} ",
      "HAVING COUNT(DISTINCT {cluster_var}) > 1 ",
      "LIMIT 1"
    )
    result = tryCatch(dbGetQuery(conn, nested_sql), error = function(e) NULL)
    
    # Query errors are treated as not nested (conservative: keeps K larger)
    if (!is.null(result) && nrow(result) == 0) {
      # FE is nested; count its levels
      count_sql = glue(
        "SELECT COUNT(DISTINCT {f}) AS n FROM (SELECT * {from_statement}) t"
      )
      n_levels = tryCatch(dbGetQuery(conn, count_sql)[["n"]], error = function(e) 0L)
      nested_df = nested_df + max(n_levels - 1L, 0L)
    }
  }
  
  nested_df
}
