#' tidy results
#' @export
generics::tidy

#' Tidiers for `dbreg` objects
#' @description Provides \code{broom::tidy} and \code{broom::glance} methods for
#' "dbreg" objects. 
#' @importFrom generics tidy
#' @importFrom stats confint
#' @param x a model of class `dbreg` produced by the \code{\link[dbreg]{dbreg}}
#' function.
#' @param conf.int Logical indicating whether to include confidence intervals.
#' Default is `FALSE`.
#' @param conf.level Confidence level for intervals. Default is 0.95.
#' @param fe Should the fixed effects be tidied too? Default is `FALSE`.
#' @param ... Additional arguments to tidying method. Currently unused except to
#' handle superseded arguments.
#' @examples
#' mod = dbreg(Temp ~ Wind | Month, data = airquality)
#' tidy(mod, conf.int = TRUE)
#' tidy(mod, conf.int = TRUE, fe = TRUE)
#' glance(mod)
#' @name tidiers
#' @rdname tidiers
#' @export
tidy.dbreg = function(x, conf.int = FALSE, conf.level = 0.95, fe = FALSE, ...) {
  # superseded args handled through ...
  dots = list(...)
  if (length(dots)) {
    if (!is.null(dots[["fes"]]) && !identical(fe, dots[["fes"]])) {
      fe = dots[["fes"]]
      warning(
        'The `fes` argument has been superseded by `fe` (without the "s") and will be deprecated in a future `dbreg` release.\n'
      )
    }
  }
  ct = x[["coeftable"]]
  if (!isTRUE(fe) && !is.null(x$fe)) {
    coef_names = x[["coef_names"]]
    if (!is.null(coef_names)) {
      ct = ct[coef_names, , drop = FALSE]
    } else {
      xvars = x[["xvars"]]
      ct = ct[xvars, , drop = FALSE]
    }
  }
  out = data.frame(term = rownames(ct), ct, row.names = NULL)
  
  if (isTRUE(conf.int)) {
    ci = confint(x, level = conf.level, fe = fe)
    out$conf.low = ci[, 1]
    out$conf.high = ci[, 2]
  }
  
  return(out)
}

#' glance results
#' @export
generics::glance

#' @importFrom generics glance
#' @rdname tidiers
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
