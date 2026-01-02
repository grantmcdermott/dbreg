
#' Plot method for dbbinsreg objects
#' 
#' @md
#' @description
#' Visualizes binned regression results from \code{\link{dbbinsreg}}.
#' Plots dots at bin means with optional confidence intervals, and optionally
#' overlays a smooth line if computed. Uses tinyplot for rendering but works
#' with both \code{plot()} and \code{tinyplot()} generics.
#' 
#' @param x A `dbbinsreg` object
#' @param type The type of plot. If `NULL` (the default), then the type will be
#' inferred based on the underlying object (e.g, `"pointrange"` for points with
#' confidence intervals).
#' @param ci Logical. Show confidence intervals for dots? Default is `TRUE.`
#' @param line Logical. Show the line overlay if available? Default is `TRUE.`
#' @param ... Additional arguments passed to `\code{\link[tinyplot]{tinyplot}},
#' e.g. `theme`, `main`, etc.
#' @export
plot.dbbinsreg = function(x, type = NULL, ci = TRUE, line = TRUE, ...) {
  # Extract metadata
  opt = x$opt
  x_var = opt$x_var
  y_var = opt$y_var

  # Start with dots (the main binscatter points)
  if (!is.null(x$data.dots)) {
    dots = x$data.dots

    # If CI requested and available, use pointrange
    if (ci && all(c("lwr", "upr") %in% names(dots)) && !all(is.na(dots$lwr))) {
      if (is.null(type)) type = "pointrange"
      tinyplot::tinyplot(
        fit ~ x,
        data = dots,
        ymin = dots$lwr,
        ymax = dots$upr,
        type = type,
        xlab = x_var,
        ylab = y_var,
        ...
      )
    } else {
      tinyplot::tinyplot(
        fit ~ x,
        data = dots,
        type = type,
        xlab = x_var,
        ylab = y_var,
        ...
      )
    }
    
    # Overlay line if available and requested
    if (line && !is.null(x$data.line)) {
      line_data = x$data.line
      # graphics::lines(line_data$x, line_data$fit, col = "steelblue", lwd = 2)
      tinyplot::tinyplot_add(
        fit ~ x, data = line_data,
        ymin = NULL, ymax = NULL,
        type = "l", lwd = 2, col = "steelblue"
      )
    }
  } else if (!is.null(x$data.line)) {
    # No dots, just show line
    if (is.null(type)) type = "l"
    line_data = x$data.line
    tinyplot::tinyplot(
      fit ~ x,
      data = line_data,
      type = type,
      xlab = x_var,
      ylab = y_var,
      ...
    )
  } else {
    warning("No data to plot (neither data.dots nor data.line available)")
  }
  
  invisible(x)
}

#' @rdname plot.dbbinsreg
#' @export
tinyplot.dbbinsreg = plot.dbbinsreg
