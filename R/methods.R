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

#' Predict method for dbreg objects
#'
#' @param object A `dbreg` object.
#' @param newdata Data frame for predictions. Required for objects that were
#'   estimated using the `"mundlak"` and `"moments"` strategies, since `dbreg`
#'   does not retain any data for these estimations.
#' @param interval description
#' @param ... Additional arguments (currently unused).
#' @importFrom stats model.matrix
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

  if (is.null(newdata)) {
    if (strategy == "compress" && !is.null(object$data)) {
      newdata = object$data
    } else {
      stop("newdata is required for predictions, as dbreg does not retain the original data.")
    }
  }
  betas = coef(object, fes = TRUE)
  fml = object$fml
  # browser()
  if (strategy == "mundlak") {
    # Mundlak: use stored group means from training to demean and predict
    fes = object$fes
    xvars = object$xvars
    yvar = object$yvar
    gm = object$group_means
    
    if (length(fes) == 1) {
      # Merge stored means with newdata
      fe1_name = fes[1]
      means_df = gm$fe1
      idx = match(newdata[[fe1_name]], means_df[[fe1_name]])
      
      # Demean X using stored means
      mm = sapply(xvars, \(v) newdata[[v]] - means_df[[paste0(v, "_mean")]][idx])
      # Get y group mean for prediction
      y_group_mean = means_df[[paste0(yvar, "_mean")]][idx]
      
    } else {
      # 2-FE: merge unit and time means
      fe1_name = fes[1]
      fe2_name = fes[2]
      idx1 = match(newdata[[fe1_name]], gm$fe1[[fe1_name]])
      idx2 = match(newdata[[fe2_name]], gm$fe2[[fe2_name]])
      browser()
      # Demean X using stored means
      mm = sapply(xvars, \(v) {
        newdata[[v]] - gm$fe1[[paste0(v, "_u")]][idx1] - 
                       gm$fe2[[paste0(v, "_t")]][idx2] + 
                       gm$overall[[paste0(v, "_o")]]
      })
      # Get y group mean for prediction
      y_group_mean = gm$fe1[[paste0(yvar, "_u")]][idx1] + 
                     gm$fe2[[paste0(yvar, "_t")]][idx2] - 
                     gm$overall[[paste0(yvar, "_o")]]
    }
    
  } else {
    # compress/moments: use model matrix with FE dummies
    fe_cols = object$fes
    # ensure fixed-effects columns are all factors
    for (col in fe_cols) {
      newdata[[col]] = factor(newdata[[col]])
    }
    # browser()
    rhs = seq_len(length(fml)[2])
    mm = model.matrix(fml, data = newdata, rhs = rhs)
  }

  # Generate predictions
  fit = as.vector(mm %*% betas)
  if (strategy == "mundlak") {
    fit = fit + y_group_mean
  }
 
  if (interval != "none") {
    vcovm = vcov(object)
    dof = object$df_residual
    if (interval == "confidence") {
      ses = sqrt(Matrix::rowSums((mm %*% vcovm) * mm))
    } else if (interval == "prediction") {
      residuals = newdata[["mean_Y"]] - fit ## FIXME!!
      sig2 = c(Matrix::crossprod(residuals)) / dof
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

