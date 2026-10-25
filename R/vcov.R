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
#' - `vcov_type = "cluster"`: sum over clusters g of `s_g s_g'`, where `s_g`
#'   is the sum of `w_i e_i s_i` over observations i in cluster g
#'
#' All residuals are scored in a single query. For OLS the score columns are
#' the regressors; for IV they are the instruments (shared by the 2SLS fit and
#' its first stages), while each residual is formed from its own regressors.
#'
#' @param inputs Inputs environment (for `conn`, `vcov_type_req` and
#'   `cluster_var`). Returns `NULL` when `vcov_type_req` is `"iid"`, since no
#'   meat matrix is needed.
#' @param frame Estimation frame (for `cte_sql`, `cte_name` and
#'   `weights_expr`); see [build_base_frame()].
#' @param resid_sql Named character vector of SQL residual expressions.
#' @param score_sql,score_names SQL expressions and names for the score columns.
#' @param score_intercept Logical. Include a constant score column (first).
#' @return Named list of meat matrices (one per residual) with dimnames
#'   `c("(Intercept)"?, score_names)`. Clustered meats carry `n_clusters`.
#' @keywords internal
compute_score_meat = function(
  inputs,
  frame,
  resid_sql,
  score_sql,
  score_names = score_sql,
  score_intercept = FALSE
) {
  env2env(inputs, environment(), c("conn", "cluster_var"))
  cte_sql = frame[["cte_sql"]]
  cte_name = frame[["cte_name"]]
  weights_expr = frame[["weights_expr"]]
  vcov_type = inputs[["vcov_type_req"]]
  if (!vcov_type %in% c("hc1", "cluster")) {
    return(NULL)
  }
  as_double = function(x) sql_cast_double(x, conn)
  resid = as_double(resid_sql)
  s_sql = c(if (score_intercept) "1", score_sql)
  s_names = c(if (score_intercept) "(Intercept)", score_names)
  s_term = ifelse(s_sql == "1", NA_character_, as_double(s_sql))
  idx = seq_along(s_sql)
  sql_prod = function(...) {
    x = c(...)
    paste(x[!is.na(x)], collapse = " * ")
  }

  if (vcov_type == "hc1") {
    w_sq = if (is.null(weights_expr)) NULL else as_double(sprintf("(%s) * (%s)", weights_expr, weights_expr))
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
    w = if (is.null(weights_expr)) NULL else as_double(weights_expr)
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
compute_meat_cluster_compress = function(inputs, from_statement, group_cols, compressed_dat, X, yhat) {
  cells = query_cluster_cells(inputs, from_statement, group_cols)
  cluster_meat_from_cells(cells, compressed_dat, group_cols, inputs[["cluster_var"]], X, yhat)
}

# Use compute_meat_cluster_compress_sql()? It pushes one literal per cell and
# column into the query, which pays off for small designs with many clusters
# but not for large designs, where fetching (cluster, cell) rows is cheaper.
# Missing cell keys can't be matched by the join.
use_cluster_sql = function(n_cells, n_cols, inputs) {
  n_cols <= 100 && n_cells * n_cols <= 20000 && isTRUE(inputs[["drop_missings"]])
}

#' Compressed cluster meats via a score pass in the database
#'
#' Pushes each cell's score columns and fitted values to the database, forms
#' each row's scores `z * w * e` (one set per fit), sums them by cluster and
#' returns their cross-products, i.e. the cluster meats. Only one group per
#' cluster is formed and only the meats' entries are returned, which beats
#' fetching one row per (cluster, cell) when there are many clusters but few
#' cells.
#' @param cells_from FROM clause exposing the cell columns (`group_cols`).
#' @param cell Cell ids of the rows of `Z` (`ROW_NUMBER()` over the cells,
#'   ordered by `group_cols`, as in the compress query).
#' @param Z Cell-level score columns, shared by all fits (the regressors for
#'   OLS; the instruments for IV and its first stages).
#' @param yhat List of cell-level fitted values, one per fit.
#' @param y_cell List of cell-level outcomes, one per fit: `NULL` for the
#'   model outcome (residuals are formed row by row), or a cell column (e.g. a
#'   first-stage outcome in compressed IV), whose residuals are constant
#'   within cells.
#' @return List of meat matrices, one per fit.
#' @keywords internal
compute_meat_cluster_compress_sql = function(inputs, cells_from, group_cols, cell, Z, yhat, y_cell = vector("list", length(yhat))) {
  env2env(inputs, environment(), c("conn", "yvar", "cluster_var", "weights"))
  # Explicitly double: some backends read decimal literals as exact DECIMALs,
  # whose products can overflow
  num = function(x) sql_cast_double(sprintf("%.17g", x), conn)
  Z = as.matrix(Z)
  k = seq_len(ncol(Z))
  fits = seq_along(yhat)
  # Per cell: score columns and, per fit, the fitted value (row-level
  # residuals) or the cell residual itself
  offsets = Map(function(yh, yc) if (is.null(yc)) yh else yc - yh, yhat, y_cell)
  cols = c(
    lapply(k, function(j) sprintf("%s AS dbreg_z%d", num(Z[, j]), j)),
    lapply(fits, function(f) sprintf("%s AS dbreg_a%d", num(offsets[[f]]), f))
  )
  fitted_sql = paste(
    sprintf("SELECT %d AS dbreg_cell, %s", cell, do.call(paste, c(cols, sep = ", "))),
    collapse = " UNION ALL\n"
  )
  w = if (is.null(weights)) NULL else gsub(weights, paste0("s.", weights), sql_weight_expr(weights), fixed = TRUE)
  e = vapply(fits, function(f) {
    r = if (is.null(y_cell[[f]])) sprintf("(s.%s - f.dbreg_a%d)", yvar, f) else sprintf("f.dbreg_a%d", f)
    if (is.null(w)) r else sprintf("(%s) * %s", w, r)
  }, character(1))
  fk = expand.grid(k = k, f = fits)
  ij = which(upper.tri(diag(length(k)), diag = TRUE), arr.ind = TRUE)
  fij = data.frame(f = rep(fits, each = nrow(ij)), i = ij[, 1], j = ij[, 2])
  gc = paste(group_cols, collapse = ", ")
  sql = paste0(
    "WITH fitted AS (\n", fitted_sql, "\n),\n",
    "cells AS (\n  SELECT ", gc, ", ROW_NUMBER() OVER (ORDER BY ", gc, ") AS dbreg_cell\n",
    "  FROM (SELECT ", gc, " ", cells_from, " GROUP BY ", gc, ") dbreg_d\n),\n",
    "scores AS (\n  SELECT s.", cluster_var, " AS dbreg_cl, ",
    paste(sprintf("SUM(f.dbreg_z%d * %s) AS u%d_%d", fk[["k"]], e[fk[["f"]]], fk[["f"]], fk[["k"]]), collapse = ", "), "\n",
    "  FROM (SELECT * ", cells_from, ") s\n",
    "  JOIN cells c ON ", paste(sprintf("s.%s = c.%s", group_cols, group_cols), collapse = " AND "), "\n",
    "  JOIN fitted f ON c.dbreg_cell = f.dbreg_cell\n",
    "  GROUP BY s.", cluster_var, "\n)\n",
    "SELECT COUNT(*) AS n_clusters, ",
    paste(sprintf("SUM(u%d_%d * u%d_%d) AS m%d_%d_%d", fij[["f"]], fij[["i"]], fij[["f"]], fij[["j"]], fij[["f"]], fij[["i"]], fij[["j"]]), collapse = ", "),
    "\nFROM scores"
  )
  res = dbGetQuery(conn, sql)
  lapply(fits, function(f) {
    meat = matrix(0, ncol(Z), ncol(Z), dimnames = list(colnames(Z), colnames(Z)))
    meat[ij] = unlist(res[1, sprintf("m%d_%d_%d", f, ij[, 1], ij[, 2])], use.names = FALSE)
    meat[ij[, 2:1]] = meat[ij]
    attr(meat, "n_clusters") = res[["n_clusters"]]
    meat
  })
}

#' Cluster-by-cell sufficient statistics for compressed cluster meats
#'
#' Returns one row per (cluster, cell) with the row count `n_gc`, the summed
#' weights `sum_w_gc` (the count if unweighted) and the weighted outcome sum
#' `sum_wy_gc`.
#' @keywords internal
query_cluster_cells = function(inputs, from_statement, group_cols) {
  env2env(inputs, environment(), c("conn", "yvar", "cluster_var", "weights"))
  group_cols_sql = paste(group_cols, collapse = ", ")
  weights_expr = sql_weight_expr(weights)
  sql = paste0(
    "SELECT ", cluster_var, ", ", group_cols_sql, ",\n",
    "  COUNT(*) AS n_gc,\n",
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

  # Cluster scores s_g = sum over the cluster's cells of X_cell * u_sum_gc,
  # for all clusters at once via a sparse cluster-indicator matrix (works for
  # dense and sparse X), then meat = sum_g s_g s_g'
  cluster_id = as.integer(factor(cells[[cluster_var]], exclude = NULL))
  indicator = Matrix::sparseMatrix(i = seq_along(cluster_id), j = cluster_id, x = 1)
  scores = crossprod(indicator, Diagonal(x = u_sum_gc) %*% X[row, , drop = FALSE])
  meat_mat = as.matrix(crossprod(scores))
  dimnames(meat_mat) = list(colnames(X), colnames(X))
  attr(meat_mat, "n_clusters") = ncol(indicator)
  meat_mat
}
