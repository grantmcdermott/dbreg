
#' Plot method for dbbinsreg objects (binsreg-style)
#' 
#' @md
#' @description
#' Visualizes binned regression results from \code{\link{dbbinsreg}} using tinyplot.
#' Plots dots at bin means with optional confidence intervals, and optionally
#' overlays a smooth line if computed.
#' 
#' @param x A dbbinsreg object
#' @param y Ignored (for S3 consistency)
#' @param ci Logical. Show confidence intervals for dots? Default is TRUE.
#' @param line Logical. Show the line overlay if available? Default is TRUE.
#' @param ... Additional arguments passed to tinyplot
#' @export
plot.dbbinsreg = function(x, y = NULL, ci = TRUE, line = TRUE, ...) {
  
  # Extract metadata
  opt = x$opt
  x_var = opt$x_var
  y_var = opt$y_var
  
  # Start with dots (the main binscatter points)
  if (!is.null(x$data.dots)) {
    dots = x$data.dots
    
    # If CI requested and available, use errorbar
    if (ci && all(c("ci.l", "ci.r") %in% names(dots)) && !all(is.na(dots$ci.l))) {
      tinyplot::tinyplot(
        fit ~ x,
        data = dots,
        type = "p",
        ymin = dots$ci.l,
        ymax = dots$ci.r,
        pch = 19,
        xlab = x_var,
        ylab = y_var,
        ...
      )
    } else {
      tinyplot::tinyplot(
        fit ~ x,
        data = dots,
        type = "p",
        pch = 19,
        xlab = x_var,
        ylab = y_var,
        ...
      )
    }
    
    # Overlay line if available and requested
    if (line && !is.null(x$data.line)) {
      line_data = x$data.line
      graphics::lines(line_data$x, line_data$fit, col = "steelblue", lwd = 2)
    }
  } else if (!is.null(x$data.line)) {
    # No dots, just show line
    line_data = x$data.line
    tinyplot::tinyplot(
      fit ~ x,
      data = line_data,
      type = "l",
      xlab = x_var,
      ylab = y_var,
      ...
    )
  } else {
    warning("No data to plot (neither data.dots nor data.line available)")
  }
  
  invisible(x)
}
