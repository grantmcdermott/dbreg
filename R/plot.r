
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


#' Plot method for dbevent objects
#'
#' @md
#' @description
#' Visualizes event study results from \code{\link{dbevent}}.
#' Plots coefficient estimates with confidence intervals across event time,
#' with a horizontal reference line at zero and a vertical line at the
#' reference period (omitted dummy).
#'
#' @param x A `dbevent` object
#' @param type The type of plot. Default is `"pointrange"` for points with
#'   confidence intervals. Can also be `"errorbar"`, `"ribbon"`, etc.
#' @param ci Logical. Show confidence intervals? Default is `TRUE`.
#' @param ref_line Logical. Show horizontal reference line at y=0? Default is `TRUE`.
#' @param ref_period_line Logical. Show vertical line at reference period? Default is `TRUE`.
#' @param xlab Character. X-axis label. Default is `"Event Time"`.
#' @param ylab Character. Y-axis label. Default is `"Estimate"`.
#' @param main Character. Plot title. Default is `"Event Study"`.
#' @param ... Additional arguments passed to \code{\link[tinyplot]{tinyplot}},
#'   e.g. `theme`, `file`, `col`, etc.
#' @return Invisibly returns the input object.
#' @examples
#' \dontrun{
#' # After running dbevent()
#' es <- dbevent(con, "panel_data", "outcome", "treat", "unit", "time")
#' plot(es)
#' 
#' # Customize appearance
#' plot(es, type = "errorbar", main = "Treatment Effects Over Time")
#' }
#' @export
plot.dbevent = function(
  x,
  type = "pointrange",
  ci = TRUE,
  ref_line = TRUE,
  ref_period_line = TRUE,
  xlab = "Event Time",
  ylab = "Estimate",
  main = "Event Study",
  ...
) {
  # Get coefficients data frame
  coef_df = x$coefs
  
  if (is.null(coef_df) || nrow(coef_df) == 0) {
    warning("No event study coefficients to plot")
    return(invisible(x))
  }
  
  # Build the plot
  if (ci && all(c("conf.low", "conf.high") %in% names(coef_df))) {
    # Plot with confidence intervals
    tinyplot::tinyplot(
      estimate ~ event_time,
      data = coef_df,
      ymin = coef_df$conf.low,
      ymax = coef_df$conf.high,
      type = type,
      xlab = xlab,
      ylab = ylab,
      main = main,
      ...
    )
  } else {
    # Plot without confidence intervals
    tinyplot::tinyplot(
      estimate ~ event_time,
      data = coef_df,
      type = "p",
      xlab = xlab,
      ylab = ylab,
      main = main,
      ...
    )
  }
  
  # Add reference line at y = 0
  if (ref_line) {
    tinyplot::tinyplot_add(
      type = "hline",
      yintercept = 0,
      lty = 2,
      col = "gray50"
    )
  }
  
  # Add vertical line at reference period
  if (ref_period_line && !is.null(x$ref)) {
    tinyplot::tinyplot_add(
      type = "vline",
      xintercept = x$ref[1],
      lty = 2,
      col = "red"
    )
  }
  
  invisible(x)
}

#' @rdname plot.dbevent
#' @export
tinyplot.dbevent = plot.dbevent
