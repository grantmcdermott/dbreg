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
      stop("Clustered SEs require a meat matrix from compute_score_meat()")
    }
    n_clusters = attr(meat, "n_clusters") # G
    if (is.null(n_clusters)) {
      stop("Meat matrix missing n_clusters attribute")
    }
    if (is.null(n_params)) n_params = ncol(XtX_inv)
    # CR1 small-sample correction: (G/(G-1)) * ((N-1)/(N-K)), as in fixest
    scale_cr1 = (n_clusters / (n_clusters - 1)) * ((nobs_orig - 1) / (nobs_orig - n_params))
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

#' Linear predictor as a SQL expression
#'
#' Pushes fitted coefficients back into the database as `b0 + b1*x1 + ...`.
#' This is the shared prediction scaffolding behind the score pass (residuals
#' for robust/clustered meat matrices), and can equally power out-of-sample
#' predictions or residualization steps in other estimators.
#'
#' @param x_sql Character vector of SQL column expressions.
#' @param beta Numeric coefficients aligned with `x_sql`.
#' @param intercept Optional numeric intercept.
#' @keywords internal
sql_linear_predictor = function(x_sql, beta, intercept = NULL) {
  terms = c(
    if (!is.null(intercept)) sprintf("%.17g", intercept),
    if (length(x_sql)) sprintf("%.17g * %s", beta, x_sql)
  )
  if (!length(terms)) {
    return("0")
  }
  paste0("(", paste(terms, collapse = " + "), ")")
}

#' Compute HC1 or cluster-robust meat matrix via a SQL score pass
#'
#' Forms residuals `e = y - x'b` in SQL (via [sql_linear_predictor()]) and
#' aggregates the score vectors `w * e * s`, where `s` are the score columns:
#' - `vcov_type = "hc1"`: `sum_i w_i^2 e_i^2 s_i s_i'`
#' - `vcov_type = "cluster"`: `sum_g (sum_{i in g} w_i e_i s_i)(...)'`
#'
#' For OLS the score columns are the regressors themselves; for IV they are
#' the instruments, while the residual is still formed from the regressors.
#' When `intercept` is supplied, a constant score column is included first.
#'
#' @param vcov_type One of `"iid"`, `"hc1"`, `"cluster"`. Returns `NULL` for
#'   `"iid"`, since no meat matrix is needed.
#' @param y_sql SQL expression for the outcome.
#' @param x_sql,beta,intercept Regressor columns and coefficients used to form
#'   the residual. See [sql_linear_predictor()].
#' @param score_sql,score_names SQL expressions and names for the score columns.
#' @param weights_expr Optional SQL weight expression.
#' @param cluster_var Cluster variable (required for `"cluster"`).
#' @return Meat matrix with dimnames `c("(Intercept)"?, score_names)`. For
#'   clustered meat, the number of clusters is attached as `n_clusters`.
#' @keywords internal
compute_score_meat = function(
  conn,
  cte_sql,
  cte_name,
  vcov_type,
  y_sql,
  x_sql,
  beta,
  intercept = NULL,
  score_sql = x_sql,
  score_names = score_sql,
  weights_expr = NULL,
  cluster_var = NULL
) {
  if (!vcov_type %in% c("hc1", "cluster")) {
    return(NULL)
  }
  as_float = function(x) sprintf("CAST(%s AS FLOAT)", x)
  resid = as_float(sprintf("(%s - %s)", y_sql, sql_linear_predictor(x_sql, beta, intercept)))
  # Score columns, with the constant first when the model has an intercept
  s_sql = c(if (!is.null(intercept)) "1", score_sql)
  s_names = c(if (!is.null(intercept)) "(Intercept)", score_names)
  idx = seq_along(s_sql)
  sql_prod = function(...) paste(c(...), collapse = " * ")

  if (vcov_type == "hc1") {
    w_sq = if (is.null(weights_expr)) NULL else as_float(sprintf("(%s) * (%s)", weights_expr, weights_expr))
    pairs = which(upper.tri(diag(length(idx)), diag = TRUE), arr.ind = TRUE)
    terms = sprintf(
      "SUM(%s) AS meat_%d_%d",
      vapply(seq_len(nrow(pairs)), function(k) {
        i = pairs[k, 1]
        j = pairs[k, 2]
        sql_prod(resid, resid, if (s_sql[i] != "1") as_float(s_sql[i]), if (s_sql[j] != "1") as_float(s_sql[j]), w_sq)
      }, character(1)),
      pairs[, 1],
      pairs[, 2]
    )
    sql = paste0(cte_sql, ",\nmeat AS (SELECT ", paste(terms, collapse = ", "), " FROM ", cte_name, ")\nSELECT * FROM meat")
  } else {
    w = if (is.null(weights_expr)) NULL else as_float(weights_expr)
    terms = sprintf(
      "SUM(%s) AS score_%d",
      vapply(idx, function(i) sql_prod(resid, if (s_sql[i] != "1") as_float(s_sql[i]), w), character(1)),
      idx
    )
    sql = paste0(
      cte_sql,
      ",\ncluster_scores AS (\n  SELECT ", cluster_var, ", ", paste(terms, collapse = ", "),
      "\n  FROM ", cte_name, "\n  GROUP BY ", cluster_var, "\n)\nSELECT * FROM cluster_scores"
    )
  }
  # Athena FLOAT gotcha
  if (inherits(conn, "AthenaConnection")) {
    sql = gsub("FLOAT", "REAL", sql, fixed = TRUE)
  }
  res = dbGetQuery(conn, sql)

  if (vcov_type == "hc1") {
    meat = matrix(0, length(idx), length(idx), dimnames = list(s_names, s_names))
    for (k in seq_len(nrow(pairs))) {
      i = pairs[k, 1]
      j = pairs[k, 2]
      meat[i, j] = meat[j, i] = res[[sprintf("meat_%d_%d", i, j)]]
    }
  } else {
    scores = as.matrix(res[, paste0("score_", idx), drop = FALSE])
    meat = crossprod(scores)
    dimnames(meat) = list(s_names, s_names)
    attr(meat, "n_clusters") = nrow(res)
  }
  meat
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
