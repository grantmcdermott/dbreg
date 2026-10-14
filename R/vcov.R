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

#' Compute HC1 or cluster-robust meat matrices via a SQL score pass
#'
#' Aggregates the score vectors `w * e * s` for one or more residuals `e`
#' (SQL expressions, typically `y - x'b` built with [sql_linear_predictor()])
#' that share the same score columns `s`:
#' - `vcov_type = "hc1"`: `sum_i w_i^2 e_i^2 s_i s_i'`
#' - `vcov_type = "cluster"`: `sum_g (sum_{i in g} w_i e_i s_i)(...)'`
#'
#' All residuals are scored in a single query. For OLS the score columns are
#' the regressors; for IV they are the instruments (shared by the 2SLS fit and
#' its first stages), while each residual is formed from its own regressors.
#'
#' @param vcov_type One of `"iid"`, `"hc1"`, `"cluster"`. Returns `NULL` for
#'   `"iid"`, since no meat matrix is needed.
#' @param resid_sql Named character vector of SQL residual expressions.
#' @param score_sql,score_names SQL expressions and names for the score columns.
#' @param score_intercept Logical. Include a constant score column (first).
#' @param weights_expr Optional SQL weight expression.
#' @param cluster_var Cluster variable (required for `"cluster"`).
#' @return Named list of meat matrices (one per residual) with dimnames
#'   `c("(Intercept)"?, score_names)`. Clustered meats carry `n_clusters`.
#' @keywords internal
compute_score_meat = function(
  conn,
  cte_sql,
  cte_name,
  vcov_type,
  resid_sql,
  score_sql,
  score_names = score_sql,
  score_intercept = FALSE,
  weights_expr = NULL,
  cluster_var = NULL
) {
  if (!vcov_type %in% c("hc1", "cluster")) {
    return(NULL)
  }
  as_float = function(x) sprintf("CAST(%s AS FLOAT)", x)
  resid = as_float(resid_sql)
  s_sql = c(if (score_intercept) "1", score_sql)
  s_names = c(if (score_intercept) "(Intercept)", score_names)
  s_term = ifelse(s_sql == "1", NA_character_, as_float(s_sql))
  idx = seq_along(s_sql)
  sql_prod = function(...) {
    x = c(...)
    paste(x[!is.na(x)], collapse = " * ")
  }

  if (vcov_type == "hc1") {
    w_sq = if (is.null(weights_expr)) NULL else as_float(sprintf("(%s) * (%s)", weights_expr, weights_expr))
    pairs = which(upper.tri(diag(length(idx)), diag = TRUE), arr.ind = TRUE)
    terms = unlist(lapply(seq_along(resid), function(r) {
      sprintf(
        "SUM(%s) AS meat_%d_%d_%d",
        vapply(seq_len(nrow(pairs)), function(k) {
          sql_prod(resid[r], resid[r], s_term[pairs[k, 1]], s_term[pairs[k, 2]], w_sq)
        }, character(1)),
        r, pairs[, 1], pairs[, 2]
      )
    }))
    sql = paste0(cte_sql, ",\nmeat AS (SELECT ", paste(terms, collapse = ", "), " FROM ", cte_name, ")\nSELECT * FROM meat")
  } else {
    w = if (is.null(weights_expr)) NULL else as_float(weights_expr)
    terms = unlist(lapply(seq_along(resid), function(r) {
      sprintf(
        "SUM(%s) AS score_%d_%d",
        vapply(idx, function(i) sql_prod(resid[r], s_term[i], w), character(1)),
        r, idx
      )
    }))
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

  meats = lapply(seq_along(resid), function(r) {
    if (vcov_type == "hc1") {
      meat = matrix(0, length(idx), length(idx), dimnames = list(s_names, s_names))
      for (k in seq_len(nrow(pairs))) {
        i = pairs[k, 1]
        j = pairs[k, 2]
        meat[i, j] = meat[j, i] = res[[sprintf("meat_%d_%d_%d", r, i, j)]]
      }
    } else {
      scores = as.matrix(res[, sprintf("score_%d_%d", r, idx), drop = FALSE])
      meat = crossprod(scores)
      dimnames(meat) = list(s_names, s_names)
      attr(meat, "n_clusters") = nrow(res)
    }
    meat
  })
  names(meats) = names(resid_sql)
  meats
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
  cells = query_cluster_cells(conn, from_statement, group_cols, yvar, cluster_var, weights)
  cluster_meat_from_cells(cells, compressed_dat, group_cols, cluster_var, X, yhat)
}

#' Cluster-by-cell sufficient statistics for compressed cluster meats
#'
#' Returns one row per (cluster, cell) with the summed weights `sum_w_gc` (the
#' count if unweighted) and the weighted outcome sum `sum_wy_gc`.
#' @keywords internal
query_cluster_cells = function(conn, from_statement, group_cols, yvar, cluster_var, weights = NULL) {
  group_cols_sql = paste(group_cols, collapse = ", ")
  weights_expr = sql_weight_expr(weights)
  sql = paste0(
    "SELECT ", cluster_var, ", ", group_cols_sql, ",\n",
    if (is.null(weights_expr)) "  COUNT(*) AS sum_w_gc,\n" else paste0("  SUM(", weights_expr, ") AS sum_w_gc,\n"),
    "  ", sql_weighted_sum(yvar, weights_expr, "sum_wy_gc"), "\n",
    from_statement, "\n",
    "GROUP BY ", cluster_var, ", ", group_cols_sql
  )
  dbGetQuery(conn, sql)
}

#' Cluster meat from cluster-by-cell statistics
#'
#' @param cells Result of [query_cluster_cells()].
#' @param X Cell-level score matrix (rows aligned with `compressed_dat`).
#' @param yhat Cell-level fitted values.
#' @param y_cell Optional cell-level outcome column. Use when the outcome is
#'   itself constant within cells (e.g. a first-stage outcome in compressed
#'   IV), in which case `cells$sum_wy_gc` is not needed.
#' @keywords internal
cluster_meat_from_cells = function(cells, compressed_dat, group_cols, cluster_var, X, yhat, y_cell = NULL) {
  # Cell keys for matching (pasted rather than interaction(), which
  # enumerates every level combination)
  cell_key = function(df) do.call(paste, c(unname(as.list(df[, group_cols, drop = FALSE])), sep = "\r"))
  cell_to_row = setNames(seq_len(nrow(compressed_dat)), cell_key(compressed_dat))
  row = cell_to_row[cell_key(cells)]

  # Summed residuals per (cluster, cell): sum_wy_gc - sum_w_gc * yhat
  sum_wy_gc = if (is.null(y_cell)) cells[["sum_wy_gc"]] else cells[["sum_w_gc"]] * y_cell[row]
  u_sum_gc = sum_wy_gc - cells[["sum_w_gc"]] * yhat[row]

  # For each cluster, compute score vector and add outer product to meat
  clusters = unique(cells[[cluster_var]])
  meat_mat = matrix(0, ncol(X), ncol(X), dimnames = list(colnames(X), colnames(X)))
  for (g in clusters) {
    in_g = cells[[cluster_var]] %in% g
    s_g = as.numeric(crossprod(X[row[in_g], , drop = FALSE], u_sum_gc[in_g]))
    meat_mat = meat_mat + tcrossprod(s_g)
  }
  attr(meat_mat, "n_clusters") = length(clusters)
  meat_mat
}
