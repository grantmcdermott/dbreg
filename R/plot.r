
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
#' @param lty Integer or string. Line type for line overlay.
#' @param ... Additional arguments passed to `\code{\link[tinyplot]{tinyplot}},
#' e.g. `theme`, `main`, `file`, etc.
#' @inherit dbbinsreg examples
#' @export
plot.dbbinsreg = function(x, type = NULL, ci = TRUE, cb = TRUE, line = TRUE, lty = 1, ...) {
  # Extract metadata
  opt = x$opt
  x_var = opt$x_var
  y_var = opt$y_var
  
  # Grab plot dimensions up front (so added layers don't get clipped)
  has_pts = !is.null(x[["points"]])
  has_ln = !is.null(x[["line"]])
  if (!(has_pts || has_ln)) {
    warning("No data to plot (neither points nor line available)")
    return(invisible(x))
  }
  has_ci = ci && has_pts && all(c("lwr", "upr") %in% names(x[["points"]])) && !all(is.na(x[["points"]][["lwr"]]))
  has_cb = cb && has_pts && all(c("cb_lwr", "cb_upr") %in% names(x[["points"]])) && !all(is.na(x[["points"]][["cb_lwr"]]))
  pts_cols = c("fit", if (has_ci) c("lwr", "upr"), if (has_cb) c("cb_lwr", "cb_upr"))
  pts_range = if (has_pts) range(x[["points"]][, pts_cols], na.rm = TRUE, finite = TRUE)
  ln_range = if (has_ln) range(x[["line"]][, "fit"], na.rm = TRUE, finite = TRUE)
  y_lim = range(c(pts_range, ln_range), na.rm = TRUE, finite = TRUE)
  x_lim = range(c(
    if (has_pts) x[["points"]][["x"]],
    if (has_ln) x[["line"]][["x"]]
  ), na.rm = TRUE, finite = TRUE)

  pts = if (has_pts) x[["points"]] else data.frame(x = NA, fit = NA, lwr = NA, upr = NA)

  # Start with empty layer
  tinyplot::tinyplot(
    fit ~ x,
    data = pts,
    xlim = x_lim,
    ylim = y_lim,
    xlab = x_var,
    ylab = y_var,
    type = "n",
    na.action = stats::na.omit,
    ...
  )

  if (has_cb) {
    tinyplot::tinyplot_add(
      ymin = pts$cb_lwr,
      ymax = pts$cb_upr,
      type = "ribbon",
      lty = 0
    )
  }

  if (has_ln) {
    # smooth_line = !is.null(x$knot) && (is.atomic(x$knot) || )
    line_params = x[["opt"]][["line"]]
    smooth_line = length(line_params) == 2 && line_params[2] > 0
    if (smooth_line) {
      tinyplot::tinyplot_add(
        data = x[["line"]],
        type = "l",
        lty = lty
      )
    } else {
      # For non-smooth lines we use a trick of inserting NAs in between bins,
      # so that each bin is plotted separately.
      line_data = x[["line"]]
      idx = unlist(lapply(split(seq_len(nrow(line_data)), line_data$bin), function(x) c(x, NA)))
      line_data = line_data[idx[-length(idx)], ]
      tinyplot::tinyplot_add(
        data = line_data,
        type = "l",
        lty = lty,
        na.action = stats::na.pass
      )
    }
  }

  if (has_pts) {
    type = if (!is.null(type)) type else if (has_ci) "pointrange" else "p"
    tinyplot::tinyplot_add(
      type = type,
      ymin = pts$lwr,
      ymax = pts$upr
    )
  }
  
  invisible(x)
}

#' @rdname plot.dbbinsreg
#' @importFrom tinyplot tinyplot
#' @export
tinyplot.dbbinsreg = plot.dbbinsreg
