#' tidy results
#' @export
generics::tidy

#' tidy `dbreg` objects
#'
#' @importFrom generics tidy
#' @param x a model of class `dbreg` produced by the [dbreg()] function
#' @param fes Should the fixed effects be tidied? Default is `FALSE`.
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
