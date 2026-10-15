##
## Stats methods for dbreg objects
##

#' Extract coefficients from dbreg objects
#'
#' @param object A `dbreg` object.
#' @param fe Should the fixed effects be included? Default is `FALSE`.
#' @param ... Additional arguments. Currently unused, except to capture
#' superseded arguments.
#' @examples
#' mod = dbreg(Temp ~ Wind | Month, data = airquality)
#' 
#' # coefficients
#' coef(mod)
#' coef(mod, fe = TRUE)  # include fixed effects
#' 
#' # confidence intervals
#' confint(mod)
#' 
#' # variance-covariance matrix
#' vcov(mod)
#' 
#' # predictions
#' head(predict(mod, newdata = airquality))
#' head(predict(mod, newdata = airquality, interval = "confidence"))
#' @export
coef.dbreg = function(object, fe = FALSE, ...) {
  
  # superseded args handled through ...
  dots = list(...)
  if (length(dots)) {
    if (!is.null(dots[["fes"]]) && !identical(fe, dots[["fes"]])) {
      fe = dots[["fes"]]
      warning('The `fes` argument has been superseded by `fe` (without the "s") and will be deprecated in a future `dbreg` release.\n')

    }
  }
  
  ct = object[["coeftable"]]
  
  if (!isTRUE(fe) && !is.null(object[["fe"]])) {
    coef_names = object[["coef_names"]]
    if (!is.null(coef_names)) {
      ct = ct[coef_names, , drop = FALSE]
    } else {
      xvars = object[["xvars"]]
      ct = ct[xvars, , drop = FALSE]
    }
  }
  
  out = ct[, "estimate"]
  names(out) = rownames(ct)
  out
}

#' Predict method for dbreg objects
#'
#' @param object A `dbreg` object.
#' @param newdata Data frame for predictions. Required for objects that were
#'   estimated using the `"mundlak"` and `"moments"` strategies, since `dbreg`
#'   does not retain any data for these estimations.
#' @param interval Type of interval to compute: `"none"` (default), 
#'   `"confidence"`, or `"prediction"`. Note that `"confidence"` intervals
#'   reflect uncertainty in the estimated mean, while `"prediction"` intervals
#'   additionally account for residual variance. See
#'   \code{\link[stats]{predict.lm}} for details.
#' @param level Confidence level for intervals. Default is 0.95.
#' @param ... Additional arguments (currently unused).
#' 
#' @section Predicting on "demean" strategy objects:
#' 
#' Predicting on `dbreg` objects should generally work as expected. However,
#' predictions from `"demean"` strategy models carry two important caveats:
#' 
#' 1. Predictions require group means to transform back to the original scale.
#' If `newdata` contains the outcome variable, group means are computed from
#' `newdata` and used to return level predictions. If the outcome is absent,
#' within-group predictions (deviations from group means) are returned instead,
#' with a message.
#' 
#' 2. Confidence/prediction intervals are not supported. A demeaned model cannot
#' account for uncertainty in the fixed-effects (since these were absorbed at
#' estimation time), which in turn would yield intervals that are too narrow.
#' Requesting intervals for `"demean"` strategy models will return point
#' predictions with a message. Users should re-estimate with a different
#' strategy if intervals are needed.
#'
#' @seealso [dbreg()] for examples.
#' @inherit coef.dbreg examples
#' @importFrom stats model.matrix reformulate ave update as.formula coef vcov qt
#' @importFrom Matrix sparse.model.matrix
#' @export
predict.dbreg = function(
  object,
  newdata = NULL,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  ...
) {
  interval = match.arg(interval)
  strategy = object[["strategy"]] 

  # Demean strategy doesn't support intervals (FE uncertainty not available)
  if (strategy == "demean" && interval != "none") {
    message(
      "Confidence/prediction intervals not supported for demean strategy ",
      "(fixed effect uncertainty unavailable). Returning point predictions."
    )
    interval = "none"
  }

  if (is.null(newdata)) {
    if (strategy == "compress" && !is.null(object[["data"]])) {
      newdata = object[["data"]]
    } else {
      stop("newdata is required for predictions, as dbreg does not retain the original data.")
    }
  }

  # Demean predictions recover the FE contribution via (double) demeaning,
  # which only covers one or two FE
  if (strategy == "demean" && length(object[["fe"]]) > 2) {
    stop(
      "predict() is not yet supported for demean models with more than two fixed effects.",
      call. = FALSE
    )
  }

  # Extract common components from object
  betas = coef(object, fe = TRUE)
  fml = object[["fml"]]
  fe = object[["fe"]]
  yvar = object[["yvar"]]
  intercept = "(Intercept)" %in% names(betas)

  # Ensure FE columns are factors
  for (f in fe) {
    newdata[[f]] = factor(newdata[[f]])
  }

  # Rebuild the design from the model's own terms (so factors, interactions and
  # no-intercept models expand exactly as in estimation), then align its
  # columns to the coefficients by name
  terms_rhs = attr(terms(formula(fml, lhs = 0, rhs = 1)), "term.labels")
  if (strategy %in% c("compress", "moments")) {
    # FE enter as dummies (compress)
    terms_rhs = c(terms_rhs, fe)
  }
  if (!length(terms_rhs)) {
    terms_rhs = "1"
  }
  fml_mm = reformulate(terms_rhs, intercept = intercept)
  # (sparse.model.matrix() can't handle backticked, non-syntactic names)
  mm = tryCatch(
    sparse.model.matrix(fml_mm, data = newdata),
    error = function(e) model.matrix(fml_mm, data = newdata)
  )
  if (strategy == "mundlak") {
    # Group means of each design column, one set per FE
    mm_means = mapply(
      function(term, f) ave(as.numeric(mm[, term]), newdata[[f]], FUN = function(y) mean(y, na.rm = TRUE)),
      object[["mundlak_means"]][["term"]],
      object[["mundlak_means"]][["fe"]]
    )
    mm_means = matrix(mm_means, nrow = nrow(mm), dimnames = list(NULL, object[["mundlak_means"]][["name"]]))
    mm = cbind(mm, mm_means)
  }
  missing_cols = setdiff(names(betas), colnames(mm))
  if (length(missing_cols)) {
    stop(
      "predict() could not rebuild the design for coefficient(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  mm = mm[, names(betas), drop = FALSE]

  if (strategy == "demean") {
    # demean: sweep the FE out of the regressors (and recover the outcome's
    # group means, if available, to return level predictions)
    has_y = yvar %in% names(newdata)
    mean_fn = function(y) mean(y, na.rm = TRUE)
    sweep_fe = function(v) {
      if (length(fe) == 1) {
        v - ave(v, newdata[[fe[1]]], FUN = mean_fn)
      } else {
        v - ave(v, newdata[[fe[1]]], FUN = mean_fn) - ave(v, newdata[[fe[2]]], FUN = mean_fn) + mean_fn(v)
      }
    }
    mm = apply(as.matrix(mm), 2, sweep_fe)
    if (is.null(dim(mm))) {
      mm = matrix(mm, nrow = 1)
    }
    if (has_y) {
      y_group_mean = newdata[[yvar]] - sweep_fe(newdata[[yvar]])
    } else {
      y_group_mean = 0
      message(sprintf(
        "Outcome '%s' not found in newdata. Returning within-group predictions (demean strategy only).",
        yvar
      ))
    }
  }

  # Generate predictions
  fit = as.vector(mm %*% betas)
  if (strategy == "demean") {
    fit = fit + y_group_mean
  }
 
  if (interval != "none") {
    vcovm = vcov(object)
    dof = object[["df_residual"]]
    if (interval == "confidence") {
      ses = sqrt(Matrix::rowSums((mm %*% vcovm) * mm))
    } else if (interval == "prediction") {
      # Use stored RSS from training to estimate sigma^2
      rss = attr(object[["vcov"]], "rss")
      if (is.null(rss)) {
        stop("Prediction intervals require RSS, which is not stored in this model.")
      }
      sig2 = rss / dof
      ses = sqrt(Matrix::rowSums((mm %*% vcovm) * mm) + sig2)
    }
    a = (1 - level) / 2
    t_crit = qt(1 - a, dof)
    lwr = fit - t_crit * ses
    upr = fit + t_crit * ses
    fit = data.frame(fit = fit, lwr = lwr, upr = upr)
  }

  return(fit)
}

#' Variance-covariance matrix for dbreg objects
#'
#' @param object A `dbreg` object.
#' @param ... Additional arguments (currently unused).
#' @inherit coef.dbreg examples
#' @export
vcov.dbreg = function(object, ...) {
  object[["vcov"]]
}

#' Confidence intervals for dbreg objects
#'
#' @param object A `dbreg` object.
#' @param parm a specification of which parameters are to be given confidence
#'   intervals, either a vector of numbers or a vector of names. If missing,
#'   all parameters are considered.
#' @param level the confidence level required. Default is 0.95.
#' @param fe Should the fixed effects be included? Default is `FALSE`.
#' @param ... Additional arguments. Currently unused, except to capture
#' superseded arguments.
#' @inherit coef.dbreg examples
#' @importFrom stats qt
#' @export
confint.dbreg = function(object, parm, level = 0.95, fe = FALSE, ...) {

  # superseded args handled through ...
  dots = list(...)
  if (length(dots)) {
    if (!is.null(dots[["fes"]]) && !identical(fe, dots[["fes"]])) {
      fe = dots[["fes"]]
      warning('The `fes` argument has been superseded by `fe` (without the "s") and will be deprecated in a future `dbreg` release.\n')

    }
  }

  ct = object[["coeftable"]]
  
  if (!isTRUE(fe) && !is.null(object[["fe"]])) {
    coef_names = object[["coef_names"]]
    if (!is.null(coef_names)) {
      ct = ct[coef_names, , drop = FALSE]
    } else {
      xvars = object[["xvars"]]
      ct = ct[xvars, , drop = FALSE]
    }
  }
  
  cf = ct[, "estimate"]
  ses = ct[, "std.error"]
  df = object[["df_residual"]]
  
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

