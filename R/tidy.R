#' tidy results
#' @export
generics::tidy

#' tidy `dbreg` objects
#'
#' @importFrom generics tidy
#' @param x a model of class `dbreg` produced by the [dbreg()] function
#' @param fes Should the fixed effects be tidied too? Default is `FALSE`.
#' @param ... Additional arguments to tidying method.
#' @export
tidy.dbreg = function(x, fes = FALSE, ...) {
  ct = x[["coeftable"]]
  if (!isTRUE(fes) && !is.null(x$fes)) {
      xvars = x[["xvars"]]
      ct = ct[xvars, , drop = FALSE]
  }
  out = data.frame(term = rownames(ct), ct, row.names = NULL)
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
