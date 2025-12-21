##
## Stats methods for dbreg objects
##

#' Extract coefficients from dbreg objects
#'
#' @param object A `dbreg` object.
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

#' Predict method for dbreg objects
#'
#' @param object A `dbreg` object.
#' @param newdata Data frame for predictions. Required for objects that were
#'   estimated using the `"mundlak"` and `"moments"` strategies, since `dbreg`
#'   does not retain any data for these estimations.
#' @param interval Type of interval to compute: `"none"` (default), 
#'   `"confidence"`, or `"prediction"`. Note that `"confidence"`` intervals
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
    if (strategy == "compress" && !is.null(object$data)) {
      newdata = object$data
    } else {
      stop("newdata is required for predictions, as dbreg does not retain the original data.")
    }
  }

  # Extract common components from object
  betas = coef(object, fes = TRUE)
  fml = object$fml
  fes = object$fes
  xvars = object$xvars
  yvar = object$yvar

  # Ensure FE columns are factors
  for (fe in fes) {
    newdata[[fe]] = factor(newdata[[fe]])
  }

  if (strategy == "demean") {
    # demean: compute group means from newdata to demean predictors
    has_y = yvar %in% names(newdata)
    mean_fn = function(y) mean(y, na.rm = TRUE)
    
    if (length(fes) == 1) {
      fe1 = fes[1]
      # Demean X using group means from newdata
      mm = sapply(xvars, \(v) {
        x_mean = ave(newdata[[v]], newdata[[fe1]], FUN = mean_fn)
        newdata[[v]] - x_mean
      })
      
      if (has_y) {
        y_group_mean = ave(newdata[[yvar]], newdata[[fe1]], FUN = mean_fn)
      } else {
        y_group_mean = 0
        message(sprintf(
          "Outcome '%s' not found in newdata. Returning within-group predictions (demean strategy only).",
          yvar
        ))
      }
      
    } else {
      # 2-FE: double demeaning
      fe1 = fes[1]
      fe2 = fes[2]
      
      # Demean X using double-demeaning from newdata
      mm = sapply(xvars, \(v) {
        x_u = ave(newdata[[v]], newdata[[fe1]], FUN = mean_fn)
        x_t = ave(newdata[[v]], newdata[[fe2]], FUN = mean_fn)
        x_o = mean_fn(newdata[[v]])
        newdata[[v]] - x_u - x_t + x_o
      })
      
      if (has_y) {
        y_u = ave(newdata[[yvar]], newdata[[fe1]], FUN = mean_fn)
        y_t = ave(newdata[[yvar]], newdata[[fe2]], FUN = mean_fn)
        y_o = mean_fn(newdata[[yvar]])
        y_group_mean = y_u + y_t - y_o
      } else {
        y_group_mean = 0
        message(sprintf(
          "Outcome '%s' not found in newdata. Returning within-group predictions (demean strategy only).",
          yvar
        ))
      }
    }
    
  } else if (strategy == "mundlak") {
    # Create group means for Mundlak prediction
    gmeans = c()
    for (x in xvars) {
      for (fe in fes) {
        demean_x = paste0(x, "_mean_", fe)
        gmeans = c(gmeans, demean_x)
        newdata[[demean_x]] = ave(
          newdata[[x]],
          newdata[[fe]],
          FUN = function(y) mean(y, na.rm = TRUE)
        )
      }
    }

    # recast as Mundlak formula
    fml_xvars = formula(fml, lhs = 0, rhs = 1)
    fml = update(fml_xvars, as.formula(paste("~ . +", paste(gmeans, collapse = " + "))))

    mm = sparse.model.matrix(fml, data = newdata)
  } else {
    # compress/moments: use sparse model matrix with FE dummies
    mm = sparse.model.matrix(reformulate(c(xvars, fes)), data = newdata)
  }

  # Generate predictions
  fit = as.vector(mm %*% betas)
  if (strategy == "demean") {
    fit = fit + y_group_mean
  }
 
  if (interval != "none") {
    vcovm = vcov(object)
    dof = object$df_residual
    if (interval == "confidence") {
      ses = sqrt(Matrix::rowSums((mm %*% vcovm) * mm))
    } else if (interval == "prediction") {
      # Use stored RSS from training to estimate sigma^2
      rss = attr(object$vcov, "rss")
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
#' @param fes Should the fixed effects be included? Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#' @importFrom stats qt
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

