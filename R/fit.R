# Estimators on sufficient statistics
#
# Strategies aggregate a single cross-moment matrix S in SQL; estimators then
# work purely on (sub-blocks of) S. Each fit_*() returns the same shape:
#   coef, bread, rss, x (regressors kept), scores (score columns for the
#   sandwich meat), project_meat (maps score-space meat to coefficient space)
# so the score pass and vcov scaling are shared by every estimator.

#' Assemble a cross-moment matrix from aggregated SQL sums
#'
#' Reads the `sum_w*` columns produced by [build_weighted_moment_terms()] into
#' one symmetric matrix over `c("(Intercept)"?, vars, y)`. Any (sub-)regression
#' can then be solved by indexing, e.g. `S[x, x]` and `S[x, y]`.
#'
#' @param moment_df One-row data frame of aggregated moments.
#' @param vars Variable names, in the order passed to
#'   [build_weighted_moment_terms()].
#' @param y Name for the outcome row/column.
#' @param intercept Logical. Include the constant (requires the linear sums).
#' @keywords internal
cross_moments = function(moment_df, vars, y, intercept = TRUE) {
  nms = c(if (intercept) "(Intercept)", vars, y)
  S = matrix(0, length(nms), length(nms), dimnames = list(nms, nms))
  S[y, y] = moment_df[["sum_wy_sq"]]
  for (i in seq_along(vars)) {
    S[vars[i], y] = S[y, vars[i]] = moment_df[[sprintf("sum_w%d_y", i)]]
    for (j in seq_len(i)) {
      S[vars[i], vars[j]] = S[vars[j], vars[i]] = moment_df[[sprintf("sum_w%d_%d", j, i)]]
    }
  }
  if (intercept) {
    S[1, 1] = moment_df[["sum_w"]]
    S[1, y] = S[y, 1] = moment_df[["sum_wy"]]
    for (i in seq_along(vars)) {
      S[1, vars[i]] = S[vars[i], 1] = moment_df[[sprintf("sum_w%d", i)]]
    }
  }
  S
}

# RSS from moments: y'y - 2b'X'y + b'X'Xb
moment_rss = function(S, y, x, beta) {
  as.numeric(S[y, y] - 2 * crossprod(beta, S[x, y]) + crossprod(beta, S[x, x] %*% beta))
}

# TSS from moments: centered if S carries an intercept, else uncentered
moment_tss = function(S, y) {
  if ("(Intercept)" %in% rownames(S)) {
    S[y, y] - S["(Intercept)", y]^2 / S["(Intercept)", "(Intercept)"]
  } else {
    S[y, y]
  }
}

#' OLS on sufficient statistics
#'
#' @param S Cross-moment matrix from [cross_moments()].
#' @param y Outcome name.
#' @param x Regressor names.
#' @param drop_collinear Logical. Drop collinear regressors first (via
#'   [detect_collinearity()]).
#' @param verbose Passed to [detect_collinearity()].
#' @keywords internal
fit_ols = function(S, y, x, drop_collinear = FALSE, verbose = FALSE) {
  dropped = character(0)
  if (drop_collinear) {
    collin = detect_collinearity(S[x, x, drop = FALSE], S[x, y, drop = FALSE], verbose = verbose)
    x = collin[["keep_names"]]
    dropped = collin[["drop_names"]]
  }
  sol = solve_with_fallback(S[x, x, drop = FALSE], S[x, y, drop = FALSE])
  beta = sol[["betahat"]]
  rownames(beta) = x
  list(
    coef = beta,
    bread = sol[["XtX_inv"]],
    rss = moment_rss(S, y, x, beta),
    x = x,
    dropped = dropped,
    scores = x,
    project_meat = function(meat) meat
  )
}

#' Linear GMM on sufficient statistics
#'
#' `b = (X'Z W Z'X)^-1 X'Z W Z'y` for a weight matrix `W`. With
#' `W = (Z'Z)^-1` this is 2SLS ([fit_2sls()]); with `W` the inverse of the
#' moment covariance it is (two-step) efficient GMM. The sandwich
#' `A^-1 [P Omega P'] A^-1`, with `P = X'Z W`, has the same form as OLS, with
#' instrument scores projected into coefficient space by `project_meat`.
#'
#' @inheritParams fit_ols
#' @param z Instrument names (included exogenous plus excluded instruments).
#' @param W Weight matrix over `z`.
#' @keywords internal
fit_gmm = function(S, y, x, z, W) {
  S_zx = S[z, x, drop = FALSE]
  P = t(S_zx) %*% W
  A = P %*% S_zx
  if (qr(A, tol = 1e-10)$rank < ncol(A)) {
    stop("IV system is not identified or regressors are collinear after instrumentation.")
  }
  sol = solve_with_fallback(A, P %*% S[z, y, drop = FALSE])
  beta = sol[["betahat"]]
  rownames(beta) = x
  list(
    coef = beta,
    bread = sol[["XtX_inv"]],
    rss = moment_rss(S, y, x, beta),
    x = x,
    dropped = character(0),
    scores = z,
    project_meat = function(meat) {
      if (is.null(meat)) {
        return(NULL)
      }
      out = P %*% meat %*% t(P)
      attr(out, "n_clusters") = attr(meat, "n_clusters")
      out
    }
  )
}

#' 2SLS on sufficient statistics
#'
#' Linear GMM ([fit_gmm()]) with weight matrix `(Z'Z)^-1`.
#'
#' @inheritParams fit_gmm
#' @keywords internal
fit_2sls = function(S, y, x, z) {
  S_zz = S[z, z, drop = FALSE]
  if (qr(S_zz, tol = 1e-10)$rank < ncol(S_zz)) {
    stop("Instrument matrix is rank deficient.")
  }
  fit_gmm(S, y, x, z, W = solve_with_fallback(S_zz, diag(ncol(S_zz)))[["XtX_inv"]])
}

#' Score-space meat matrices for fitted models
#'
#' Thin wrapper around [compute_score_meat()] that builds each fit's residual
#' from its coefficients and regressors, and scores all fits in one pass.
#' The fits must share the same score columns (e.g. a 2SLS fit and its first
#' stages, which all use the instruments).
#'
#' @param fits List of [fit_ols()] / [fit_2sls()] results.
#' @param frame List with `cte_sql`, `cte_name`, `cols` (named character
#'   vector mapping variable names to SQL columns in `cte_name`) and
#'   `weights_expr`.
#' @param ys Outcome names (in `frame$cols`), one per fit.
#' @param inputs Inputs environment (for `conn`, `vcov_type_req`,
#'   `cluster_var`).
#' @return List of meat matrices (or `NULL`s for iid), one per fit.
#' @keywords internal
fit_meats = function(fits, frame, ys, inputs) {
  scores = fits[[1]][["scores"]]
  if (!all(vapply(fits, function(f) identical(f[["scores"]], scores), logical(1)))) {
    stop("fit_meats() requires fits that share the same score columns.")
  }
  cols = frame[["cols"]]
  resid_sql = vapply(seq_along(fits), function(k) {
    b = fits[[k]][["coef"]][, 1]
    slopes = setdiff(fits[[k]][["x"]], "(Intercept)")
    sprintf(
      "(%s - %s)",
      cols[[ys[k]]],
      sql_linear_predictor(unname(cols[slopes]), b[slopes], if ("(Intercept)" %in% fits[[k]][["x"]]) b[["(Intercept)"]])
    )
  }, character(1))
  score_cols = setdiff(scores, "(Intercept)")
  meats = compute_score_meat(
    inputs = inputs,
    frame = frame,
    resid_sql = resid_sql,
    score_sql = unname(cols[score_cols]),
    score_names = score_cols,
    score_intercept = "(Intercept)" %in% scores
  )
  if (is.null(meats)) vector("list", length(fits)) else unname(meats)
}

#' @rdname fit_meats
#' @param fit,y A single fit and its outcome name.
fit_meat = function(fit, frame, y, inputs) {
  fit_meats(list(fit), frame, y, inputs)[[1]]
}
