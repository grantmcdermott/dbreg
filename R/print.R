#' Print method for dbreg objects
#' @param x `dbreg` object.
#' @param fe Should the fixed effects be displayed? Default is `FALSE`.
#' @param ... Other arguments passed to \code{\link[base]{print}}. Currently
#' unused, except to capture superseded arguments.
#' @examples
#' mod = dbreg(Temp ~ Wind | Month, data = airquality)
#' # mod # same as below
#' print(mod)
#' print(mod, fe = TRUE)  # include fixed effects
#' @export
print.dbreg = function(x, fe = FALSE, ...) {
  
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
  colnames(ct) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  if (!isTRUE(fe) && !is.null(x$fe)) {
    xvars = x[["xvars"]]
    ct = ct[xvars, , drop = FALSE]
  }
  se_type = attr(x$vcov, "type")
  n_clusters = attr(x$vcov, "n_clusters")
  se_type = switch(
    se_type,
    "iid" = "IID",
    "hc1" = "Heteroskedasticity-robust",
    "cluster" = if (!is.null(n_clusters)) {
      sprintf("Clustered (%d clusters)", n_clusters)
    } else {
      "Clustered"
    }
  )
  if (x$strategy == "compress") {
    cat("Compressed OLS estimation, Dep. Var.:", x$yvar, "\n")
    cat(
      "Observations.:",
      prettyNum(x$nobs_orig, big.mark = ","),
      "(original) |",
      prettyNum(x$nobs, big.mark = ","),
      "(compressed)",
      "\n"
    )
  } else if (x$strategy == "demean") {
    n_fe = length(x$fe)
    if (n_fe == 1) {
      mstring = "Demeaned"
    } else {
      mstring = "Double Demeaned"
    }
    cat(paste(mstring, "OLS estimation, Dep. Var.:", x$yvar, "\n"))
    cat("Observations.:", prettyNum(x$nobs_orig, big.mark = ","), "\n")
  } else if (x$strategy == "mundlak") {
    n_fe = length(x$fe)
    mstring = "Mundlak"
    if (n_fe == 1) {
      mstring = paste("One-way", mstring)
    } else if (n_fe == 2) {
      mstring = paste("Two-way", mstring)
    } else if (n_fe > 2) {
      mstring = paste0(n_fe, "-way ", mstring)
    }
    cat(paste(mstring, "OLS estimation, Dep. Var.:", x$yvar, "\n"))
    cat("Observations.:", prettyNum(x$nobs_orig, big.mark = ","), "\n")
  } else if (x$strategy == "moments") {
    cat("Moments-based OLS estimation, Dep. Var.:", x$yvar, "\n")
    cat("Observations.:", prettyNum(x$nobs_orig, big.mark = ","), "\n")
  }
  cat("Standard Errors:", se_type, "\n")

  # Calculate goodness-of-fit metrics
  gof_vals = gof(x)

  print_coeftable(ct, gof_vals = gof_vals, has_fes = !is.null(x$fe))
  invisible(ct)
}

# stolen from Laurent here:
# https://github.com/lrberge/fixest/blob/5523d48ef4a430fa2e82815ca589fc8a47168fe7/R/miscfuns.R#L3758
print_coeftable = function(
  coeftable,
  lastLine = "",
  show_signif = TRUE,
  gof_vals = NULL,
  has_fes = FALSE
) {
  # Simple function that does as the function coeftable but handles special cases
  # => to take care of the case when the coefficient is bounded

  if (!is.data.frame(coeftable)) {
    class(coeftable) = NULL
    ct = as.data.frame(coeftable)
  } else {
    ct = coeftable
  }

  signifCode = c("***" = 0.001, "** " = 0.01, "*  " = 0.05, ".  " = 0.1)

  pvalues = ct[, 4]

  stars = cut(
    pvalues,
    breaks = c(-1, signifCode, 100),
    labels = c(names(signifCode), "")
  )
  stars[is.na(stars)] = ""

  whoIsLow = !is.na(pvalues) & pvalues < 2.2e-16

  # Note that it's a bit different than format => I don't like xxe-yy numbers, very hard to read: you can't see large/small nbers at first sight
  for (i in 1:3) {
    ct[, i] = decimalFormat(ct[, i])
  }

  ct[!whoIsLow, 4] = format(ct[!whoIsLow, 4], digits = 5)

  ct[whoIsLow, 4] = "< 2.2e-16"
  ct[is.na(ct[, 4]), 4] = "NA"

  ct[, 5] = stars
  names(ct)[5] = ""

  print(ct)

  cat(lastLine)

  if (show_signif) {
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  }

  # Print goodness-of-fit metrics if available
  if (!is.null(gof_vals) && !all(is.na(gof_vals))) {
    rmse = gof_vals["rmse"]
    adj_r2 = gof_vals["adj_r2"]

    # Format RMSE and Adj R2 on same line like fixest
    if (!is.na(rmse) && !is.na(adj_r2)) {
      cat(sprintf(
        "RMSE: %s%sAdj. R2: %s\n",
        format(round(rmse, 1), big.mark = ",", nsmall = 1),
        strrep(" ", max(1, 20 - nchar(format(round(rmse, 1), big.mark = ",")))),
        format(round(adj_r2, 6), nsmall = 6)
      ))
    }
  }
}

decimalFormat = function(x) {
  who_valid = which(!is.na(x) & is.numeric(x))
  if (length(who_valid) == 0) {
    return(x)
  }

  res = x

  x_valid = x[who_valid]
  xPower = log10(abs(x_valid))

  if (min(xPower) > 0) {
    pow_round = max(1, 6 - ceiling(xPower))
  } else if (min(xPower) < -5) {
    pow_round = ceiling(abs(min(xPower))) + 2
  } else {
    pow_round = 6
  }

  res[who_valid] = round(x_valid, pow_round)

  res
}

#' Print method for dbbinsreg objects (binsreg-compatible format)
#' 
#' @param x A dbbinsreg object
#' @param ... Additional arguments passed to print
#' @export
print.dbbinsreg = function(x, ...) {
  opt = x$opt
  
  cat("Binscatter Plot\n")
  cat("Formula:", deparse(opt$formula), "\n")
  
  points_str = if (!is.null(opt$points)) paste0("c(", opt$points[1], ",", opt$points[2], ")") else "NULL"
  line_str = if (!is.null(opt$line)) paste0("c(", opt$line[1], ",", opt$line[2], ")") else "NULL"
  cat(sprintf("points = %s | line = %s | nbins = %d | binspos = '%s'\n", 
              points_str, line_str, opt$nbins, opt$binspos))
  
  # Get model (handles both single and dual-path structures)
  mod = if (is.list(x$model) && !is.null(x$model$points)) x$model$points else x$model
  
  if (!is.null(mod) && identical(mod$strategy, "compress")) {
    cat(sprintf("Observations: %s (original) | %s (compressed)\n",
                prettyNum(mod$nobs_orig, big.mark = ","),
                prettyNum(mod$nobs, big.mark = ",")))
  } else {
    cat(sprintf("N = %s\n", prettyNum(opt$N, big.mark = ",")))
  }
  
  invisible(x)
}

