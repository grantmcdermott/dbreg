
#' Plot method for dbbin objects
#' 
#' @md
#' @description
#' Visualizes binned regression results from \code{\link{dbbin}} using tinyplot
#' with confidence ribbon support.
#' 
#' @param x A dbbin object
#' @param y Ignored (for S3 consistency)
#' @param ci Logical. Show confidence intervals as ribbon? Default is TRUE.
#' @param ... Additional arguments passed to tinyplot
#' @export
plot.dbbin = function(x, y = NULL, ci = TRUE, ...) {
  
  # Check for tinyplot

  if (!requireNamespace("tinyplot", quietly = TRUE)) {
    stop("The plot.dbbin method requires the tinyplot package.\n",
         "Install it with: install.packages('tinyplot')",
         call. = FALSE)
  }
  
  # Extract data and metadata
  data = x$data
  x_var = x$x_var
  y_var = x$y_var
  
  # Set clean theme
  tinyplot::tinytheme("clean")
  
 # Build ymin/ymax for ribbon if CI requested and available
  if (ci && all(c("ci_low", "ci_high") %in% names(data))) {
    tinyplot::tinyplot(
      y_hat ~ x,
      data = data,
      type = "ribbon",
      ymin = data$ci_low,
      ymax = data$ci_high,
      xlab = x_var,
      ylab = y_var,
      ...
    )
  } else {
    tinyplot::tinyplot(
      y_hat ~ x,
      data = data,
      type = "l",
      xlab = x_var,
      ylab = y_var,
      ...
    )
  }
  
  invisible(x)
}
