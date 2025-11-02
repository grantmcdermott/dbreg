#' tidy results
#' @export
generics::tidy

#' tidy `dbreg` objects
#'
#' @importFrom generics tidy
#' @param x a model of class `dbreg` produced by the [dbreg()] function
#' @param conf.int Logical indicating whether to include confidence intervals.
#'   Default is `FALSE`.
#' @param conf.level Confidence level for intervals. Default is 0.95.
#' @param fes Should the fixed effects be tidied too? Default is `FALSE`.
#' @param ... Additional arguments to tidying method.
#' @export
tidy.dbreg = function(x, conf.int = FALSE, conf.level = 0.95, fes = FALSE, ...) {
  ct = x[["coeftable"]]
  if (!isTRUE(fes) && !is.null(x$fes)) {
      xvars = x[["xvars"]]
      ct = ct[xvars, , drop = FALSE]
  }
  out = data.frame(term = rownames(ct), ct, row.names = NULL)
  
  if (isTRUE(conf.int)) {
    ci = confint(x, level = conf.level, fes = fes)
    out$conf.low = ci[, 1]
    out$conf.high = ci[, 2]
  }
  
  return(out)
}

#' glance results
#' @export
generics::glance

#' glance `dbreg` objects
#'
#' @importFrom generics glance
#' @param x a model of class `dbreg` produced by the [dbreg()] function
#' @param ... Additional arguments to glancing method.
#' @export
glance.dbreg = function(x, ...) {
  gof_vals = gof(x)
  data.frame(
    r.squared = gof_vals["r2"],
    adj.r.squared = gof_vals["adj_r2"],
    rmse = gof_vals["rmse"],
    nobs = x$nobs_orig,
    df.residual = x$df_residual,
    row.names = NULL
  )
}
