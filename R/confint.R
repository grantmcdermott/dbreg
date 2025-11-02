#' Variance-covariance matrix for dbreg objects
#'
#' @param object a model of class `dbreg` produced by the [dbreg()] function
#' @param ... Additional arguments (currently unused).
#' @export
vcov.dbreg = function(object, ...) {
  object[["vcov"]]
}

#' Confidence intervals for dbreg objects
#'
#' @param object a model of class `dbreg` produced by the [dbreg()] function.
#' @param parm a specification of which parameters are to be given confidence
#'   intervals, either a vector of numbers or a vector of names. If missing,
#'   all parameters are considered.
#' @param level the confidence level required. Default is 0.95.
#' @param fes Should the fixed effects be included? Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#' @export
confint.dbreg = function(object, parm, level = 0.95, fes = FALSE, ...) {
  ct = object[["coeftable"]]
  
  if (!isTRUE(fes) && !is.null(object$fes)) {
    xvars = object[["xvars"]]
    ct = ct[xvars, , drop = FALSE]
  }
  
  cf = ct[, "estimate"]
  ses = ct[, "std.error"]
  df = object$df_residual
  
  a = (1 - level) / 2
  t_crit = qt(1 - a, df)
  ci = cbind(cf - t_crit * ses, cf + t_crit * ses)
  rownames(ci) = rownames(ct)
  colnames(ci) = sprintf("%.1f %%", 100 * c(a, 1 - a))
  
  if (!missing(parm)) {
    ci = ci[parm, , drop = FALSE]
  }
  
  ci
}
