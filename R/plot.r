
#' Plot method for dbbinsreg objects
#' 
#' @md
#' @description
#' Visualizes binned regression results from \code{\link{dbbinsreg}}.
#' Plots dots at bin means with optional confidence intervals and/or
#' confidence bands, and optionally overlays a smooth line if computed.
#' Uses tinyplot for rendering but works with both \code{plot()} and
#' \code{tinyplot()} generics.
#' 
#' @param x A `dbbinsreg` object
#' @param type The type of plot. If `NULL` (the default), then the type will be
#' inferred based on the underlying object (e.g, `"pointrange"` for points with
#' confidence intervals).
#' @param ci Logical. Show confidence intervals for dots? Default is `TRUE.`
#' @param cb Logical. Show confidence bands as a ribbon? Default is `TRUE` if
#' available in the object.
#' @param line Logical. Show the line overlay if available? Default is `TRUE.`
#' @param ... Additional arguments passed to `\code{\link[tinyplot]{tinyplot}},
#' e.g. `theme`, `main`, `file`, etc.
#' @inherit dbbinsreg examples
#' @export
plot.dbbinsreg = function(x, type = NULL, ci = TRUE, cb = TRUE, line = TRUE, ...) {
  # Extract metadata
  opt = x$opt
  x_var = opt$x_var
  y_var = opt$y_var

  # Start with points (the main binscatter points)
  if (!is.null(x$points)) {
    pts = x$points
    
    # Check if CB is available and requested
    has_cb = cb && all(c("cb_lwr", "cb_upr") %in% names(pts)) && !all(is.na(pts$cb_lwr))
    
    # Plot CB ribbon first (so it's behind the points)
    if (has_cb) {
      tinyplot::tinyplot(
        fit ~ x,
        data = pts,
        ymin = cb_lwr,
        ymax = cb_upr,
        type = "ribbon",
        xlab = x_var,
        ylab = y_var,
        lty = 0, # FIXME
        ...
      )
      # Add points/CI on top
      if (ci && all(c("lwr", "upr") %in% names(pts)) && !all(is.na(pts$lwr))) {
        if (is.null(type)) type = "pointrange"
        tinyplot::tinyplot_add(
          fit ~ x,
          data = pts,
          ymin = pts$lwr,
          ymax = pts$upr,
          type = type,
          lty = 1 # FIXME
        )
      } else {
        tinyplot::tinyplot_add(
          fit ~ x,
          data = pts,
          type = "p"
        )
      }
    } else if (ci && all(c("lwr", "upr") %in% names(pts)) && !all(is.na(pts$lwr))) {
      # CI only (no CB)
      if (is.null(type)) type = "pointrange"
      tinyplot::tinyplot(
        fit ~ x,
        data = pts,
        ymin = pts$lwr,
        ymax = pts$upr,
        type = type,
        xlab = x_var,
        ylab = y_var,
        ...
      )
    } else {
      # No CI or CB
      tinyplot::tinyplot(
        fit ~ x,
        data = pts,
        type = type,
        xlab = x_var,
        ylab = y_var,
        ...
      )
    }
    
    # Overlay line if available and requested
    if (line && !is.null(x$line)) {
      line_data = x$line
      tinyplot::tinyplot_add(
        fit ~ x, data = line_data,
        ymin = NULL, ymax = NULL,
        type = "l",
        lty = 1, # FIXME
        lwd = 2,
        col = "steelblue"
      )
    }
  } else if (!is.null(x$line)) {
    # No points, just show line
    if (is.null(type)) type = "l"
    line_data = x$line
    tinyplot::tinyplot(
      fit ~ x,
      data = line_data,
      type = type,
      xlab = x_var,
      ylab = y_var,
      lty = 1, # FIXME
      ...
    )
  } else {
    warning("No data to plot (neither points nor line available)")
  }
  
  invisible(x)
}

#' @rdname plot.dbbinsreg
#' @importFrom tinyplot tinyplot
#' @export
tinyplot.dbbinsreg = plot.dbbinsreg
