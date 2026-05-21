# Variance-covariance and meat matrix computation
# These functions are shared across all dbreg strategies.

#' Compute variance-covariance matrix
#' @keywords internal
compute_vcov = function(
  vcov_type = "iid",
  strategy = "compress",
  XtX_inv,
  rss,
  df_res,
  nobs_orig, # N
  n_params = NULL, # K (for CR1 correction)
  X = NULL,
  rss_g = NULL,
  meat = NULL
) {
  if (vcov_type == "hc1") {
    if (strategy == "compress" && is.null(meat)) {
      # Compress strategy: HC1 with grouped residuals
      meat = crossprod(X, Diagonal(x = as.numeric(rss_g)) %*% X)
    }
    # meat should be provided for demean/mundlak/moments strategies
    if (is.null(meat)) {
      stop("HC1 requires meat matrix for non-compress strategies")
    }
    scale_hc1 = nobs_orig / df_res
    vcov_mat = scale_hc1 * (XtX_inv %*% meat %*% XtX_inv)
    attr(vcov_mat, "type") = "hc1"
  } else if (vcov_type == "cluster") {
    # Cluster-robust (CR1) standard errors
    if (is.null(meat)) {
      stop("Clustered SEs require meat matrix from compute_meat_cluster_sql")
    }
    n_clusters = attr(meat, "n_clusters") # G
    if (is.null(n_clusters)) {
      stop("Meat matrix missing n_clusters attribute")
    }
    if (is.null(n_params)) n_params = ncol(XtX_inv)
    # CR1 small-sample correction: (G/(G-1)) * (N/(N-K))
    scale_cr1 = (n_clusters / (n_clusters - 1)) * (nobs_orig / (nobs_orig - n_params))
    vcov_mat = scale_cr1 * (XtX_inv %*% meat %*% XtX_inv)
    attr(vcov_mat, "type") = "cluster"
    attr(vcov_mat, "n_clusters") = n_clusters
  } else {
    # IID case (same for all strategies)
    sigma2 = rss / df_res
    vcov_mat = sigma2 * XtX_inv
    attr(vcov_mat, "type") = "iid"
  }
  dimnames(vcov_mat) = dimnames(XtX_inv)
  vcov_mat
}

#' Compute HC1 meat matrix via SQL
#' @keywords internal
compute_meat_sql = function(conn, cte_sql, vars, yvar, betahat,
                            is_athena = FALSE,
                            var_suffix = "_tilde",
                            cte_name = "demeaned",
                            has_intercept = FALSE,
                            vars_sql = NULL,
                            weights_expr = NULL) {
  # Build variable SQL expressions (use provided or construct from suffix)
  if (is.null(vars_sql)) {
    vars_sql = paste0(vars, var_suffix)
  }
  yvar_sql = paste0(yvar, var_suffix)

  # Extract beta values (betahat may be a matrix)
  beta_vals = as.numeric(betahat[vars, 1])
  weight_sql = if (is.null(weights_expr)) {
    NULL
  } else {
    sprintf("CAST((%s) * (%s) AS FLOAT)", weights_expr, weights_expr)
  }
  build_sum = function(parts, alias) {
    parts = parts[!vapply(parts, is.null, logical(1))]
    sprintf("SUM(%s) AS %s", paste(parts, collapse = " * "), alias)
  }

  # Build residual expression: y - intercept - sum(beta_j * x_j)
  if (has_intercept) {
    intercept_val = as.numeric(betahat["(Intercept)", 1])
    beta_terms = paste(
      sprintf("%.15g * %s", beta_vals, vars_sql),
      collapse = " + "
    )
    resid_expr = sprintf("(%s - %.15g - (%s))", yvar_sql, intercept_val, beta_terms)
  } else {
    beta_terms = paste(
      sprintf("%.15g * %s", beta_vals, vars_sql),
      collapse = " + "
    )
    resid_expr = sprintf("(%s - (%s))", yvar_sql, beta_terms)
  }

  # Build meat terms using numeric indices to avoid naming collisions
  # (variable names may contain underscores from interaction terms)
  meat_terms = character(0)

  if (has_intercept) {
    meat_terms = c(meat_terms, build_sum(c(
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      weight_sql
    ), "meat_0_0"))
    for (j in seq_along(vars)) {
      meat_terms = c(meat_terms, build_sum(c(
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", vars_sql[j]),
        weight_sql
      ), sprintf("meat_0_%d", j)))
    }
  }

  for (i in seq_along(vars)) {
    for (j in i:length(vars)) {
      meat_terms = c(meat_terms, build_sum(c(
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", vars_sql[i]),
        sprintf("CAST(%s AS FLOAT)", vars_sql[j]),
        weight_sql
      ), sprintf("meat_%d_%d", i, j)))
    }
  }

  meat_sql = paste0(
    cte_sql,
    ",\nmeat AS (SELECT ", paste(meat_terms, collapse = ", "), " FROM ", cte_name, ")\n",
    "SELECT * FROM meat"
  )

  if (is_athena) {
    meat_sql = gsub("FLOAT", "REAL", meat_sql, fixed = TRUE)
  }

  meat_df = dbGetQuery(conn, meat_sql)

  # Reconstruct meat matrix
  if (has_intercept) {
    vars_all = c("(Intercept)", vars)
  } else {
    vars_all = vars
  }
  p = length(vars_all)
  meat_mat = matrix(0, p, p, dimnames = list(vars_all, vars_all))

  if (has_intercept) {
    meat_mat[1, 1] = meat_df[["meat_0_0"]]
    for (j in seq_along(vars)) {
      val = meat_df[[sprintf("meat_0_%d", j)]]
      meat_mat[1, j + 1] = meat_mat[j + 1, 1] = val
    }
  }

  for (i in seq_along(vars)) {
    for (j in i:length(vars)) {
      val = meat_df[[sprintf("meat_%d_%d", i, j)]]
      idx_i = if (has_intercept) i + 1 else i
      idx_j = if (has_intercept) j + 1 else j
      meat_mat[idx_i, idx_j] = meat_mat[idx_j, idx_i] = val
    }
  }
  meat_mat
}

#' Compute cluster-robust meat matrix via SQL
#'
#' Computes the meat matrix for cluster-robust (CR0) standard errors by
#' aggregating score vectors within clusters and computing outer products.
#'
#' @keywords internal
compute_meat_cluster_sql = function(conn, cte_sql, vars, yvar, betahat,
                                    cluster_var,
                                    is_athena = FALSE,
                                    var_suffix = "_tilde",
                                    cte_name = "demeaned",
                                    has_intercept = FALSE,
                                    vars_sql = NULL,
                                    weights_expr = NULL) {
  # Build variable SQL expressions (use provided or construct from suffix)
  if (is.null(vars_sql)) {
    vars_sql = paste0(vars, var_suffix)
  }
  yvar_sql = paste0(yvar, var_suffix)

  # Extract beta values
  beta_vals = as.numeric(betahat[vars, 1])
  weight_sql = if (is.null(weights_expr)) NULL else sprintf("CAST(%s AS FLOAT)", weights_expr)
  build_sum = function(parts, alias) {
    parts = parts[!vapply(parts, is.null, logical(1))]
    sprintf("SUM(%s) AS %s", paste(parts, collapse = " * "), alias)
  }

  # Build residual expression: y - intercept - sum(beta_j * x_j)
  if (has_intercept) {
    intercept_val = as.numeric(betahat["(Intercept)", 1])
    beta_terms = paste(
      sprintf("%.15g * %s", beta_vals, vars_sql),
      collapse = " + "
    )
    resid_expr = sprintf("(%s - %.15g - (%s))", yvar_sql, intercept_val, beta_terms)
  } else {
    beta_terms = paste(
      sprintf("%.15g * %s", beta_vals, vars_sql),
      collapse = " + "
    )
    resid_expr = sprintf("(%s - (%s))", yvar_sql, beta_terms)
  }

  # Build cluster score terms using numeric indices to avoid naming collisions
  score_terms = character(0)

  if (has_intercept) {
    score_terms = c(score_terms, build_sum(c(
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      weight_sql
    ), "score_0"))
  }

  for (j in seq_along(vars)) {
    score_terms = c(score_terms, build_sum(c(
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      sprintf("CAST(%s AS FLOAT)", vars_sql[j]),
      weight_sql
    ), sprintf("score_%d", j)))
  }

  # Query: aggregate scores by cluster
  cluster_sql = paste0(
    cte_sql,
    ",\ncluster_scores AS (\n  SELECT ", cluster_var, ", ",
    paste(score_terms, collapse = ", "),
    "\n  FROM ", cte_name,
    "\n  GROUP BY ", cluster_var,
    "\n)\nSELECT * FROM cluster_scores"
  )

  if (is_athena) {
    cluster_sql = gsub("FLOAT", "REAL", cluster_sql, fixed = TRUE)
  }

  cluster_df = dbGetQuery(conn, cluster_sql)

  # Build meat matrix from cluster scores: M = sum_g(s_g %*% t(s_g))
  if (has_intercept) {
    vars_all = c("(Intercept)", vars)
  } else {
    vars_all = vars
  }
  p = length(vars_all)
  meat_mat = matrix(0, p, p, dimnames = list(vars_all, vars_all))

  # Extract score columns
  score_cols = if (has_intercept) {
    c("score_0", paste0("score_", seq_along(vars)))
  } else {
    paste0("score_", seq_along(vars))
  }

  # Sum outer products across clusters
  for (i in seq_len(nrow(cluster_df))) {
    s_g = as.numeric(cluster_df[i, score_cols])
    meat_mat = meat_mat + tcrossprod(s_g)
  }

  # Return meat matrix and number of clusters for df adjustment
  attr(meat_mat, "n_clusters") = nrow(cluster_df)
  meat_mat
}

#' Compute cluster-robust meat matrix for compress strategy
#'
#' For compress strategy, we need to query cluster-by-cell stats from the
#' original data, then compute cluster scores using the cell-level fitted values.
#'
#' @keywords internal
compute_meat_cluster_compress = function(conn, from_statement, group_cols,
                                         yvar, cluster_var, compressed_dat,
                                         X, yhat, weights = NULL) {
  group_cols_sql = paste(group_cols, collapse = ", ")
  weights_expr = sql_weight_expr(weights)

  # Query cluster-by-cell sufficient statistics
  if (is.null(weights_expr)) {
    cluster_cell_sql = paste0(
      "SELECT ", cluster_var, ", ", group_cols_sql, ",\n",
      "  COUNT(*) AS n_gc,\n",
      "  SUM(", yvar, ") AS sum_y_gc\n",
      from_statement, "\n",
      "GROUP BY ", cluster_var, ", ", group_cols_sql
    )
  } else {
    cluster_cell_sql = paste0(
      "SELECT ", cluster_var, ", ", group_cols_sql, ",\n",
      "  SUM(", weights_expr, ") AS sum_w_gc,\n",
      "  SUM((", weights_expr, ") * (", yvar, ")) AS sum_wy_gc\n",
      from_statement, "\n",
      "GROUP BY ", cluster_var, ", ", group_cols_sql
    )
  }

  cluster_cell_df = dbGetQuery(conn, cluster_cell_sql)

  # Create cell key for matching (same grouping as compress strategy)
  compressed_dat[["cell_key"]] = interaction(compressed_dat[, group_cols, drop = FALSE])
  cluster_cell_df[["cell_key"]] = interaction(cluster_cell_df[, group_cols, drop = FALSE])

  # Add yhat to compressed_dat and create lookup
  compressed_dat[["yhat"]] = yhat
  yhat_lookup = compressed_dat[, c("cell_key", "yhat")]

  # Merge to get yhat for each cluster-cell combo (only keep needed cols from cluster_cell_df)
  if (is.null(weights_expr)) {
    cluster_cell_df = merge(
      cluster_cell_df[, c("cell_key", cluster_var, "n_gc", "sum_y_gc")],
      yhat_lookup,
      by = "cell_key",
      all.x = TRUE
    )
    # Compute summed residuals per (cluster, cell): u_sum_gc = sum_y_gc - n_gc * yhat
    cluster_cell_df[["u_sum_gc"]] = cluster_cell_df[["sum_y_gc"]] - cluster_cell_df[["n_gc"]] * cluster_cell_df[["yhat"]]
  } else {
    cluster_cell_df = merge(
      cluster_cell_df[, c("cell_key", cluster_var, "sum_w_gc", "sum_wy_gc")],
      yhat_lookup,
      by = "cell_key",
      all.x = TRUE
    )
    # Compute summed residuals per (cluster, cell): u_sum_gc = sum_wy_gc - sum_w_gc * yhat
    cluster_cell_df[["u_sum_gc"]] = cluster_cell_df[["sum_wy_gc"]] - cluster_cell_df[["sum_w_gc"]] * cluster_cell_df[["yhat"]]
  }

  # Get unique clusters
  clusters = unique(cluster_cell_df[[cluster_var]])
  n_clusters = length(clusters)
  p = ncol(X)

  # Initialize meat matrix
  meat_mat = matrix(0, p, p, dimnames = list(colnames(X), colnames(X)))

  # Create cell_key to row index mapping for X matrix
  cell_to_row = setNames(seq_len(nrow(compressed_dat)), as.character(compressed_dat[["cell_key"]]))

  # For each cluster, compute score vector and add outer product to meat
  for (g in clusters) {
    cells_in_g = cluster_cell_df[cluster_cell_df[[cluster_var]] == g, ]

    # Find which rows in X correspond to these cells
    cell_matches = cell_to_row[as.character(cells_in_g[["cell_key"]])]

    # Compute s_g = X' * u_sum (weighted by u_sum_gc for each cell)
    s_g = as.numeric(crossprod(X[cell_matches, , drop = FALSE], cells_in_g[["u_sum_gc"]]))
    meat_mat = meat_mat + tcrossprod(s_g)
  }

  attr(meat_mat, "n_clusters") = n_clusters
  meat_mat
}
