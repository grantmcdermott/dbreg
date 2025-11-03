#' Extract coefficients from dbreg objects
#'
#' @param object a model of class `dbreg` produced by the [dbreg()] function
#' @param fes Should the fixed effects be included? Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#' @export
coef.dbreg = function(object, fes = FALSE, ...) {
  ct = object[["coeftable"]]
  
  if (!isTRUE(fes) && !is.null(object$fes)) {
    xvars = object[["xvars"]]
    ct = ct[xvars, , drop = FALSE]
  }
  
  out = ct[, "estimate"]
  names(out) = rownames(ct)
  out
}
