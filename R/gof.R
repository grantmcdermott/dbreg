#' Calculate goodness-of-fit metrics for dbreg objects
#'
#' @param object a model of class `dbreg` produced by the [dbreg()] function
#' @param ... Additional arguments (currently unused)
#' @return Named vector with r2, adj_r2, and rmse
#' @keywords internal
gof = function(object, ...) {
  nobs = object$nobs_orig
  df_res = object$df_residual
  
  rss = attr(object$vcov, "rss")
  tss = attr(object$vcov, "tss")
  
  if (is.null(rss) || is.null(tss)) {
    warning("RSS or TSS not available in model object.")
    return(c(r2 = NA_real_, adj_r2 = NA_real_, rmse = NA_real_))
  }
  
  r2_val = 1 - rss / tss
  adj_r2_val = 1 - (rss / df_res) / (tss / (nobs - 1))
  rmse_val = sqrt(rss / nobs)
  
  c(r2 = r2_val, adj_r2 = adj_r2_val, rmse = rmse_val)
}
