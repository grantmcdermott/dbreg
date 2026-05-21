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
  
  weights_expr = sql_weight_expr(inputs[["weights"]])
  pair_exprs = build_weighted_moment_terms(
    y_sql = inputs[["yvar"]],
    x_sql = xvars_sql,
    x_aliases = xvar_names,
    weights_expr = weights_expr,
    alias_mode = "names",
    prefix_terms = sql_count(inputs[["conn"]], "n_total")
  )
  
  # CTE structure for HC1 meat computation
  cte_sql = paste0("WITH base AS (SELECT * ", inputs[["from_statement"]], ")")
  
  moments_sql = paste0(
    cte_sql, "\n",
    "SELECT\n  ",
    paste(pair_exprs, collapse = ",\n  "),
    "\nFROM base"
  )

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
  sum_w = moments_df[["sum_w"]]

  vars_all = c("(Intercept)", xvar_names)
  p = length(vars_all)
  XtX = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  Xty = matrix(0, p, 1, dimnames = list(vars_all, ""))

  XtX["(Intercept)", "(Intercept)"] = sum_w
  Xty["(Intercept)", ] = moments_df[["sum_wy"]]
  for (x in xvar_names) {
    swx = moments_df[[paste0("sum_w", x)]]
    swxx = moments_df[[paste0("sum_w", x, "_", x)]]
    swxy = moments_df[[paste0("sum_w", x, "_y")]]
    XtX["(Intercept)", x] = XtX[x, "(Intercept)"] = swx
    XtX[x, x] = swxx
    Xty[x, ] = swxy
  }
  xpairs = gen_xvar_pairs(xvar_names)
  for (pair in xpairs) {
    xi = pair[1]
    xj = pair[2]
    val = moments_df[[paste0("sum_w", xi, "_", xj)]]
    XtX[xi, xj] = XtX[xj, xi] = val
  }

  solve_result = solve_with_fallback(XtX, Xty)
  betahat = solve_result[["betahat"]]
  XtX_inv = solve_result[["XtX_inv"]]
  rownames(betahat) = vars_all

  rss = as.numeric(
    moments_df[["sum_wy_sq"]] -
      2 * t(betahat) %*% Xty +
      t(betahat) %*% XtX %*% betahat
  )
  df_res = max(n_total - p, 1)
  # Calculate TSS for R2
  sum_wy = moments_df[["sum_wy"]]
  sum_wy_sq = moments_df[["sum_wy_sq"]]
  tss = sum_wy_sq - (sum_wy^2 / sum_w)
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = NULL
  is_athena = inherits(inputs[["conn"]], "AthenaConnection")
  if (inputs[["vcov_type_req"]] == "hc1") {
    meat = compute_meat_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      vars = xvar_names,
      vars_sql = xvars_sql,
      yvar = inputs[["yvar"]],
      betahat = betahat,
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "base",
      has_intercept = TRUE,
      weights_expr = weights_expr
    )
  } else if (inputs[["vcov_type_req"]] == "cluster") {
    meat = compute_meat_cluster_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      vars = xvar_names,
      vars_sql = xvars_sql,
      yvar = inputs[["yvar"]],
      betahat = betahat,
      cluster_var = inputs[["cluster_var"]],
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "base",
      has_intercept = TRUE,
      weights_expr = weights_expr
    )
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
    strategy = "moments",
    XtX_inv = XtX_inv,
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
  weights_expr_base = sql_weight_expr(inputs[["weights"]])
  weights_expr_demeaned = if (is.null(inputs[["weights"]])) NULL else sql_weight_expr("weights")

  all_var_names = c(yvar, xvar_names)
  all_var_sql = c(yvar, xvars_sql)
  
  cluster_var = inputs[["cluster_var"]]
  use_ap = FALSE
  ap_tables = NULL
  if (length(inputs[["fe"]]) >= 2) {
    if (length(inputs[["fe"]]) > 2) {
      use_ap = TRUE
    } else {
      is_balanced = inputs[["is_balanced"]]
      if (is.null(is_balanced)) {
        is_balanced = dbreg_is_balanced_panel(inputs[["conn"]], inputs[["from_statement"]], inputs[["fe"]])
      }
      use_ap = !is.null(inputs[["weights"]]) || !isTRUE(is_balanced)
    }
    if (isTRUE(use_ap) && inputs[["verbose"]]) {
      message("[dbreg] Using alternating projections for FE demeaning")
    }
    if (isTRUE(use_ap) && inputs[["sql_only"]]) {
      stop("[dbreg] sql_only is not supported for alternating projections.", call. = FALSE)
    }
  }
  if (length(inputs[["fe"]]) == 1) {
    # Single FE: simple within-group demeaning
    fe1 = inputs[["fe"]][1]
    
    # Build base CTE with expanded columns
    base_select = c(fe1, yvar)
    for (i in seq_along(xvar_names)) {
      base_select = c(base_select, sprintf("%s AS %s", xvars_sql[i], xvar_names[i]))
    }
    if (!is.null(inputs[["weights"]]) && !inputs[["weights"]] %in% c(fe1, yvar, xvar_names)) {
      base_select = c(base_select, inputs[["weights"]])
    }
    if (!is.null(cluster_var) && !cluster_var %in% c(fe1, yvar, xvar_names, inputs[["weights"]])) {
      base_select = c(base_select, cluster_var)
    }
    
    means_cols = paste(
      vapply(
        all_var_names,
        function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_mean")),
        character(1)
      ),
      collapse = ", "
    )
    tilde_exprs = paste(
      sprintf("(b.%s - gm.%s_mean) AS %s_tilde", all_var_names, all_var_names, all_var_names),
      collapse = ",\n       "
    )
    if (!is.null(inputs[["weights"]])) {
      tilde_exprs = paste(
        tilde_exprs,
        sprintf("b.%s AS weights", inputs[["weights"]]),
        sep = ",\n       "
      )
    }
    if (!is.null(cluster_var) && !cluster_var %in% c(fe1)) {
      tilde_exprs = paste(
        tilde_exprs,
        sprintf("b.%s AS %s", cluster_var, cluster_var),
        sep = ",\n       "
      )
    }

    # CTE part (reusable for HC1 meat computation)
    cte_sql = paste0(
      "WITH base AS (
      SELECT ", paste(base_select, collapse = ", "), " ",
      inputs[["from_statement"]],
      "
      ),
      group_means AS (
      SELECT ",
      fe1,
      ", ",
      means_cols,
      " FROM base GROUP BY ",
      fe1,
      "
      ),
      demeaned AS (
      SELECT
          b.",
      fe1,
      ",
          ",
      tilde_exprs,
      "
      FROM base b
      JOIN group_means gm ON b.",
      fe1,
      " = gm.",
      fe1,
      "
      )"
    )

    moment_terms = c(
      sql_count(inputs[["conn"]], "n_total"),
      sql_count(inputs[["conn"]], "n_fe1", fe1, distinct = TRUE),
      "1 AS n_fe2",
      sql_weighted_sum(
        glue("CAST({yvar}_tilde AS FLOAT) * CAST({yvar}_tilde AS FLOAT)"),
        weights_expr_demeaned,
        "sum_y_sq"
      )
    )
  } else {
    # 2 FE: AP when weighted/unbalanced; double demeaning otherwise
    fe1 = inputs[["fe"]][1]
    fe2 = inputs[["fe"]][2]

    if (isTRUE(use_ap)) {
      ap_res = dbreg_alternating_projections(
        conn = inputs[["conn"]],
        from_statement = inputs[["from_statement"]],
        fe = inputs[["fe"]],
        yvar = yvar,
        xvars_sql = xvars_sql,
        xvar_names = xvar_names,
        weights = inputs[["weights"]],
        cluster_var = cluster_var,
        verbose = inputs[["verbose"]]
      )
      ap_tables = c(ap_res[["table"]], ap_res[["base_table"]])
      weights_expr_demeaned = "__w"
      cte_sql = paste0("WITH demeaned AS (SELECT * FROM ", ap_res[["table"]], ")")

      fe_count_terms = vapply(seq_along(inputs[["fe"]]), function(k) {
        sql_count(inputs[["conn"]], sprintf("n_fe%d", k), inputs[["fe"]][k], distinct = TRUE)
      }, character(1))
      moment_terms = c(
        sql_count(inputs[["conn"]], "n_total"),
        fe_count_terms,
        sql_weighted_sum(
          glue("CAST({yvar}_tilde AS FLOAT) * CAST({yvar}_tilde AS FLOAT)"),
          weights_expr_demeaned,
          "sum_y_sq"
        )
      )
    } else {
      # Double demeaning (balanced panels, unweighted)
      base_select = c(fe1, fe2, yvar)
      for (i in seq_along(xvar_names)) {
        base_select = c(base_select, sprintf("%s AS %s", xvars_sql[i], xvar_names[i]))
      }
      if (!is.null(inputs[["weights"]]) && !inputs[["weights"]] %in% c(fe1, fe2, yvar, xvar_names)) {
        base_select = c(base_select, inputs[["weights"]])
      }
      if (!is.null(cluster_var) && !cluster_var %in% c(fe1, fe2, yvar, xvar_names, inputs[["weights"]])) {
        base_select = c(base_select, cluster_var)
      }

      unit_means_cols = paste(
        vapply(
          all_var_names,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_u")),
          character(1)
        ),
        collapse = ", "
      )
      time_means_cols = paste(
        vapply(
          all_var_names,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_t")),
          character(1)
        ),
        collapse = ", "
      )
      overall_cols = paste(
        vapply(
          all_var_names,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_o")),
          character(1)
        ),
        collapse = ", "
      )
      tilde_exprs = paste(
        sprintf(
          "(b.%s - um.%s_u - tm.%s_t + o.%s_o) AS %s_tilde",
          all_var_names,
          all_var_names,
          all_var_names,
          all_var_names,
          all_var_names
        ),
        collapse = ",\n       "
      )
      if (!is.null(cluster_var) && !cluster_var %in% c(fe1, fe2)) {
        tilde_exprs = paste(
          tilde_exprs,
          sprintf("b.%s AS %s", cluster_var, cluster_var),
          sep = ",\n       "
        )
      }
      if (!is.null(inputs[["weights"]])) {
        tilde_exprs = paste(
          tilde_exprs,
          sprintf("b.%s AS weights", inputs[["weights"]]),
          sep = ",\n       "
        )
      }

      # CTE part (reusable for HC1 meat computation)
      cte_sql = paste0(
        "WITH base AS (
        SELECT ", paste(base_select, collapse = ", "), " ",
        inputs[["from_statement"]],
        "
        ),
        unit_means AS (
        SELECT ",
        fe1,
        ", ",
        unit_means_cols,
        " FROM base GROUP BY ",
        fe1,
        "
        ),
        time_means AS (
        SELECT ",
        fe2,
        ", ",
        time_means_cols,
        " FROM base GROUP BY ",
        fe2,
        "
        ),
        overall AS (
        SELECT ",
        overall_cols,
        " FROM base
        ),
        demeaned AS (
        SELECT
            b.",
        fe1,
        ",
            b.",
        fe2,
        ",
            ",
        tilde_exprs,
        "
        FROM base b
        JOIN unit_means um ON b.",
        fe1,
        " = um.",
        fe1,
        "
        JOIN time_means tm ON b.",
        fe2,
        " = tm.",
        fe2,
        "
        CROSS JOIN overall o
        )"
      )

      moment_terms = c(
        sql_count(inputs[["conn"]], "n_total"),
        sql_count(inputs[["conn"]], "n_fe1", fe1, distinct = TRUE),
        sql_count(inputs[["conn"]], "n_fe2", fe2, distinct = TRUE),
        sql_weighted_sum(
          glue("CAST({yvar}_tilde AS FLOAT) * CAST({yvar}_tilde AS FLOAT)"),
          weights_expr_demeaned,
          "sum_y_sq"
        )
      )
    }
  }

  # Add moment terms for xvars (shared by both 1-FE and 2-FE cases)
  # Use numeric indices for column aliases to avoid naming collisions
  for (i in seq_along(xvar_names)) {
    x = xvar_names[i]
    moment_terms = c(
      moment_terms,
      sql_weighted_sum(
        glue("CAST({x}_tilde AS FLOAT) * CAST({yvar}_tilde AS FLOAT)"),
        weights_expr_demeaned,
        sprintf("sum_%d_y", i)
      ),
      sql_weighted_sum(
        glue("CAST({x}_tilde AS FLOAT) * CAST({x}_tilde AS FLOAT)"),
        weights_expr_demeaned,
        sprintf("sum_%d_%d", i, i)
      )
    )
  }
  xpairs = gen_xvar_pairs(xvar_names)
  for (pair in xpairs) {
    i = match(pair[1], xvar_names)  # larger index (i > j in gen_xvar_pairs)
    j = match(pair[2], xvar_names)  # smaller index
    moment_terms = c(
      moment_terms,
      sql_weighted_sum(
        glue("CAST({pair[2]}_tilde AS FLOAT) * CAST({pair[1]}_tilde AS FLOAT)"),
        weights_expr_demeaned,
        sprintf("sum_%d_%d", j, i)  # store as sum_smaller_larger
      )
    )
  }

  # Build full SQL
  demean_sql = paste0(
    cte_sql,
    ",
      moments AS (
      SELECT
          ",
    paste(moment_terms, collapse = ",\n    "),
    "
      FROM demeaned
      )
      SELECT * FROM moments"
  )

  # Athena FLOAT gotcha
  # https://github.com/DyfanJones/noctua/issues/228
  if (inherits(inputs[["conn"]], "AthenaConnection")) {
    demean_sql = gsub("FLOAT", "REAL", demean_sql, fixed = TRUE)
  }

  if (inputs[["sql_only"]]) {
    return(demean_sql)
  }

  # Execute SQL and build matrices
  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbreg] Executing weighted demean SQL\n" else "[dbreg] Executing demean SQL\n")
  }
  demean_df = dbGetQuery(inputs[["conn"]], demean_sql)
  ap_cleanup = function() {
    if (!is.null(ap_tables)) {
      backend = detect_backend(inputs[["conn"]])[["name"]]
      for (tbl in ap_tables) {
        drop_table_if_exists(inputs[["conn"]], tbl, backend)
      }
    }
  }
  if (inputs[["data_only"]]) {
    ap_cleanup()
    return(demean_df)
  }
  n_total = demean_df[["n_total"]]
  n_fe_levels = vapply(seq_along(inputs[["fe"]]), function(k) {
    val = demean_df[[sprintf("n_fe%d", k)]]
    if (is.null(val)) 1L else as.integer(val)
  }, integer(1))

  p = length(xvar_names)
  XtX = matrix(0, p, p, dimnames = list(xvar_names, xvar_names))
  Xty = matrix(0, p, 1, dimnames = list(xvar_names, ""))

  for (i in seq_along(xvar_names)) {
    XtX[i, i] = demean_df[[sprintf("sum_%d_%d", i, i)]]
    Xty[i, ] = demean_df[[sprintf("sum_%d_y", i)]]
  }
  if (length(xvar_names) > 1) {
    for (i in seq_along(xvar_names)) {
      if (i == 1) next
      for (j in seq_len(i - 1)) {
        # Pairs stored as sum_j_i where j < i
        XtX[i, j] = XtX[j, i] = demean_df[[sprintf("sum_%d_%d", j, i)]]
      }
    }
  }

  # Detect and handle collinearity
  collin = detect_collinearity(XtX, Xty, verbose = inputs[["verbose"]])
  XtX = collin[["XtX"]]
  Xty = collin[["Xty"]]
  xvar_names_kept = collin[["keep_names"]]
  collin_vars = collin[["drop_names"]]

  solve_result = solve_with_fallback(XtX, Xty)
  betahat = solve_result[["betahat"]]
  XtX_inv = solve_result[["XtX_inv"]]
  rownames(betahat) = xvar_names_kept
  p_kept = length(xvar_names_kept)

  rss = as.numeric(
    demean_df[["sum_y_sq"]] -
      2 * t(betahat) %*% Xty +
      t(betahat) %*% XtX %*% betahat
  )
  n_fe = length(inputs[["fe"]])
  df_fe = sum(n_fe_levels) - (n_fe - 1)
  df_res = max(n_total - p_kept - df_fe, 1)
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = NULL
  n_params_cluster = p_kept + df_fe  # K for CR1 correction
  is_athena = inherits(inputs[["conn"]], "AthenaConnection")
  if (inputs[["vcov_type_req"]] == "hc1") {
    meat = compute_meat_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      vars = xvar_names_kept,
      yvar = yvar,
      betahat = betahat,
      is_athena = is_athena,
      weights_expr = weights_expr_demeaned
    )
  } else if (inputs[["vcov_type_req"]] == "cluster") {
    meat = compute_meat_cluster_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      vars = xvar_names_kept,
      yvar = yvar,
      betahat = betahat,
      cluster_var = inputs[["cluster_var"]],
      is_athena = is_athena,
      weights_expr = weights_expr_demeaned
    )
    # For ssc = "nested", exclude nested FE levels from K
    if (inputs[["ssc"]] == "nested") {
      nested_levels = count_nested_fe_levels(
        inputs[["conn"]], inputs[["from_statement"]], inputs[["fe"]], inputs[["cluster_var"]]
      )
      n_params_cluster = p_kept + df_fe - nested_levels
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
    strategy = "demean",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    n_params = n_params_cluster,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = demean_df[["sum_y_sq"]]

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)
  ap_cleanup()

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

  # Build moment terms using numeric indices
  moment_terms = build_weighted_moment_terms(
    y_sql = glue("CAST({yvar} AS FLOAT)"),
    x_sql = glue("CAST({all_regressors} AS FLOAT)"),
    x_aliases = all_regressors,
    weights_expr = weights_expr_aug,
    alias_mode = "indices",
    prefix_terms = c(
      sql_count(inputs[["conn"]], "n_total"),
      if (n_fe >= 1) sql_count(inputs[["conn"]], "n_fe1", fe[1], distinct = TRUE) else "1 AS n_fe1",
      if (n_fe >= 2) sql_count(inputs[["conn"]], "n_fe2", fe[2], distinct = TRUE) else "1 AS n_fe2"
    )
  )

  # CTE part (reusable for HC1 meat computation)
  cte_sql = paste0(
    "WITH base AS (SELECT ", paste(base_select, collapse = ", "), " ", inputs[["from_statement"]], "),\n",
    paste(cte_parts, collapse = ",\n"), ",\n",
    "augmented AS (SELECT ", aug_select, " FROM base b ", paste(join_parts, collapse = " "), ")"
  )

  mundlak_sql = paste0(
    cte_sql, ",\n",
    "moments AS (SELECT ", paste(moment_terms, collapse = ", "), " FROM augmented)\n",
    "SELECT * FROM moments"
  )

  # Athena FLOAT gotcha
  if (inherits(inputs[["conn"]], "AthenaConnection")) {
    mundlak_sql = gsub("FLOAT", "REAL", mundlak_sql, fixed = TRUE)
  }

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
  sum_w = mundlak_df[["sum_w"]]

  # Include intercept
  vars_all = c("(Intercept)", all_regressors)
  p = length(vars_all)

  XtX = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  Xty = matrix(0, p, 1, dimnames = list(vars_all, ""))

  # Intercept terms
  XtX[1, 1] = sum_w
  Xty[1, ] = mundlak_df[["sum_wy"]]

  # Regressor terms (using numeric indices)
  for (i in seq_along(all_regressors)) {
    XtX[1, i + 1] = XtX[i + 1, 1] = mundlak_df[[sprintf("sum_w%d", i)]]
    XtX[i + 1, i + 1] = mundlak_df[[sprintf("sum_w%d_%d", i, i)]]
    Xty[i + 1, ] = mundlak_df[[sprintf("sum_w%d_y", i)]]
  }

  # Cross-terms
  for (i in seq_along(all_regressors)) {
    for (j in seq_along(all_regressors)) {
      if (i < j) {
        XtX[i + 1, j + 1] = XtX[j + 1, i + 1] = mundlak_df[[sprintf("sum_w%d_%d", i, j)]]
      }
    }
  }

  solve_result = solve_with_fallback(XtX, Xty)
  betahat = solve_result[["betahat"]]
  XtX_inv = solve_result[["XtX_inv"]]
  rownames(betahat) = vars_all

  # RSS and TSS
  rss = as.numeric(
    mundlak_df[["sum_wy_sq"]] -
      2 * t(betahat) %*% Xty +
      t(betahat) %*% XtX %*% betahat
  )
  tss = mundlak_df[["sum_wy_sq"]] - (mundlak_df[["sum_wy"]]^2 / sum_w)

  df_res = max(n_total - p, 1)

  # Compute meat matrix if needed (HC1 or cluster)
  meat = NULL
  is_athena = inherits(inputs[["conn"]], "AthenaConnection")
  if (inputs[["vcov_type_req"]] == "hc1") {
    meat = compute_meat_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      vars = all_regressors,
      yvar = yvar,
      betahat = betahat,
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "augmented",
      has_intercept = TRUE,
      weights_expr = weights_expr_aug
    )
  } else if (inputs[["vcov_type_req"]] == "cluster") {
    meat = compute_meat_cluster_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      vars = all_regressors,
      yvar = yvar,
      betahat = betahat,
      cluster_var = inputs[["cluster_var"]],
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "augmented",
      has_intercept = TRUE,
      weights_expr = weights_expr_aug
    )
  }

  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
    strategy = "mundlak",
    XtX_inv = XtX_inv,
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
      "Consider strategy = 'mundlak'."
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
      nested_levels = count_nested_fe_levels(
        inputs[["conn"]], from_statement, inputs[["fe"]], inputs[["cluster_var"]]
      )
      n_params_cluster = ncol(X) - nested_levels
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

#' Count levels of FE variables nested within cluster variable
#' 
#' For ssc = "nested", we exclude FE levels from K if the FE is nested within
#' the cluster variable (i.e., each FE value belongs to exactly one cluster).
#' 
#' @keywords internal
count_nested_fe_levels = function(conn, from_statement, fe, cluster_var) {
  if (is.null(fe) || length(fe) == 0 || is.null(cluster_var)) {
    return(0L)
  }
  
  nested_levels = 0L
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
    
    if (is.null(result) || nrow(result) == 0) {
      # FE is nested; count its levels
      count_sql = glue(
        "SELECT COUNT(DISTINCT {f}) AS n FROM (SELECT * {from_statement}) t"
      )
      n_levels = tryCatch(dbGetQuery(conn, count_sql)[["n"]], error = function(e) 0L)
      nested_levels = nested_levels + n_levels
    }
  }
  
  nested_levels
}
