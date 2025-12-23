
#' Plot method for dbbin objects
#' 
#' @md
#' @description
#' Visualizes binned regression results from \code{\link{dbbin}}, with support for 
#' different plot types (line segments, points, or connected points) and optional 
#' confidence intervals.
#' 
#' @param x A dbbin object
#' @param y Ignored (for S3 consistency)
#' @param type Plot type: "line" (piecewise segments), "points" (bin midpoints),
#'   or "connected" (points connected with lines). Default is "line".
#' @param ci Logical. Show confidence intervals? Default is FALSE.
#' @param level Significance level for confidence intervals. Default is 0.05
#'   (95% confidence intervals). Only used when ci = TRUE.
#' @param backend Graphics backend: "auto" (use tinyplot if available, else base),
#'   "tinyplot", or "base". Default is "auto".
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.dbbin = function(x, y = NULL, 
                      type = c("line", "points", "connected"),
                      ci = FALSE,
                      level = 0.05,
                      backend = c("auto", "tinyplot", "base"), 
                      ...) {
  
  backend = match.arg(backend)
  type = match.arg(type)
  
  # Auto-detect backend
  if (backend == "auto") {
    backend = if (requireNamespace("tinyplot", quietly = TRUE)) "tinyplot" else "base"
  }
  
  degree = unique(x$degree)
  
  # If CI is requested, recalculate CIs from stored SEs using the requested level
  if (ci) {
    crit_val = stats::qnorm(1 - level / 2)
    
    if (degree == 0) {
      # Recalculate from SE
      if ("se" %in% names(x)) {
        x$ci_low = x$y - crit_val * x$se
        x$ci_high = x$y + crit_val * x$se
      }
    } else {
      # Recalculate from SEs for left/mid/right
      if ("se_left" %in% names(x)) {
        x$ci_low_left = x$y_left - crit_val * x$se_left
        x$ci_high_left = x$y_left + crit_val * x$se_left
        x$ci_low_right = x$y_right - crit_val * x$se_right
        x$ci_high_right = x$y_right + crit_val * x$se_right
        
        if ("se_mid" %in% names(x)) {
          x$ci_low_mid = x$y_mid - crit_val * x$se_mid
          x$ci_high_mid = x$y_mid + crit_val * x$se_mid
        }
      }
    }
  }
  
  # Check if tinyplot is requested but not available
  if (backend == "tinyplot" && !requireNamespace("tinyplot", quietly = TRUE)) {
    message("tinyplot not available, falling back to base graphics")
    backend = "base"
  }
  
  if (backend == "tinyplot") {
    # Use tinyplot
    tinyplot::tinytheme("clean")
    
    if (degree == 0) {
      # Bin means - step function
      if (type == "points") {
        tinyplot::tinyplot(x$x_mid, x$y, type = "p", 
                          xlab = "x", ylab = "y", ...)
      } else {
        # Step function for degree 0
        tinyplot::tinyplot(x$x_mid, x$y, type = "s", 
                          xlab = "x", ylab = "y", ...)
      }
    } else {
      # Piecewise linear/polynomial
      if (type == "points") {
        # Just show bin midpoints
        if (degree >= 2 && "y_mid" %in% names(x)) {
          y_mid_vals = x$y_mid
        } else {
          y_mid_vals = (x$y_left + x$y_right) / 2
        }
        
        # Add CI bars if requested
        if (ci && "ci_low_mid" %in% names(x)) {
           # Use segments for error bars
           tinyplot::tinyplot(x$x_mid, y_mid_vals, type = "p",
                             xlab = "x", ylab = "y", ...)
           # Add error bars manually? tinyplot might not support this easily yet
        } else {
          tinyplot::tinyplot(x$x_mid, y_mid_vals, type = "p",
                            xlab = "x", ylab = "y", ...)
        }
      } else {
        # Plot piecewise segments
        # Set up plot region first
        y_range = if (degree >= 2 && "y_mid" %in% names(x)) {
          range(c(x$y_left, x$y_mid, x$y_right), na.rm = TRUE)
        } else {
          range(c(x$y_left, x$y_right), na.rm = TRUE)
        }
        
        if (ci && "ci_low_left" %in% names(x)) {
          y_range = range(c(y_range, x$ci_low_left, x$ci_high_left, x$ci_low_right, x$ci_high_right), na.rm = TRUE)
        }
        
        tinyplot::tinyplot(range(c(x$x_left, x$x_right)), 
                          y_range,
                          type = "n", xlab = "x", ylab = "y", ...)
        
        # Draw curves/segments for each bin
        for (i in seq_len(nrow(x))) {
          if (degree >= 2 && "y_mid" %in% names(x)) {
            # Quadratic: draw smooth curve through 3 points
            x_seq = seq(x$x_left[i], x$x_right[i], length.out = 50)
            x_pts = c(x$x_left[i], x$x_mid[i], x$x_right[i])
            y_pts = c(x$y_left[i], x$y_mid[i], x$y_right[i])
            
            # Lagrange interpolation through 3 points
            y_seq = lagrange_interp_3pt(x_seq, x_pts, y_pts)
            
            # Draw CI band if requested
            if (ci && "ci_low_left" %in% names(x)) {
              # Interpolate CI bounds (approximate)
              ci_low_pts = c(x$ci_low_left[i], x$ci_low_mid[i], x$ci_low_right[i])
              ci_high_pts = c(x$ci_high_left[i], x$ci_high_mid[i], x$ci_high_right[i])
              
              ci_low_seq = lagrange_interp_3pt(x_seq, x_pts, ci_low_pts)
              ci_high_seq = lagrange_interp_3pt(x_seq, x_pts, ci_high_pts)
              
              polygon(c(x_seq, rev(x_seq)), c(ci_low_seq, rev(ci_high_seq)), 
                      col = adjustcolor("grey", alpha.f = 0.3), border = NA)
            }
            
            lines(x_seq, y_seq, ...)
            
          } else {
            # Linear: draw line segment
            if (ci && "ci_low_left" %in% names(x)) {
              polygon(c(x$x_left[i], x$x_right[i], x$x_right[i], x$x_left[i]),
                      c(x$ci_low_left[i], x$ci_low_right[i], x$ci_high_right[i], x$ci_high_left[i]),
                      col = adjustcolor("grey", alpha.f = 0.3), border = NA)
            }
            
            lines(c(x$x_left[i], x$x_right[i]),
                  c(x$y_left[i], x$y_right[i]), ...)
          }
        }
      }
    }
  } else {
    # Use base graphics
    if (degree == 0) {
      # Bin means
      if (type == "points") {
        # Setup plot
        y_range = range(x$y, na.rm = TRUE)
        if (ci && "ci_low" %in% names(x)) {
          y_range = range(c(y_range, x$ci_low, x$ci_high), na.rm = TRUE)
        }
        
        plot(x$x_mid, x$y, type = "n", xlab = "x", ylab = "y", ylim = y_range, ...)
        
        if (ci && "ci_low" %in% names(x)) {
          segments(x$x_mid, x$ci_low, x$x_mid, x$ci_high, col = "grey")
        }
        points(x$x_mid, x$y, ...)
        
      } else {
        # Step function
        # Setup plot
        y_range = range(x$y, na.rm = TRUE)
        if (ci && "ci_low" %in% names(x)) {
          y_range = range(c(y_range, x$ci_low, x$ci_high), na.rm = TRUE)
        }
        
        plot(range(c(x$x_left, x$x_right)), y_range, type = "n", xlab = "x", ylab = "y", ...)
        
        for (i in seq_len(nrow(x))) {
          if (ci && "ci_low" %in% names(x)) {
            rect(x$x_left[i], x$ci_low[i], x$x_right[i], x$ci_high[i], 
                 col = adjustcolor("grey", alpha.f = 0.3), border = NA)
          }
          lines(c(x$x_left[i], x$x_right[i]), c(x$y[i], x$y[i]), ...)
        }
      }
    } else {
      # Piecewise linear/polynomial
      if (type == "points") {
        if (degree >= 2 && "y_mid" %in% names(x)) {
          y_mid_vals = x$y_mid
        } else {
          y_mid_vals = (x$y_left + x$y_right) / 2
        }
        
        # Setup plot
        y_range = range(y_mid_vals, na.rm = TRUE)
        if (ci && "ci_low_mid" %in% names(x)) {
          y_range = range(c(y_range, x$ci_low_mid, x$ci_high_mid), na.rm = TRUE)
        }
        
        plot(x$x_mid, y_mid_vals, type = "n", xlab = "x", ylab = "y", ylim = y_range, ...)
        
        if (ci && "ci_low_mid" %in% names(x)) {
          segments(x$x_mid, x$ci_low_mid, x$x_mid, x$ci_high_mid, col = "grey")
        }
        points(x$x_mid, y_mid_vals, ...)
        
      } else {
        # Set up plot region
        y_range = if (degree >= 2 && "y_mid" %in% names(x)) {
          range(c(x$y_left, x$y_mid, x$y_right), na.rm = TRUE)
        } else {
          range(c(x$y_left, x$y_right), na.rm = TRUE)
        }
        
        if (ci && "ci_low_left" %in% names(x)) {
          y_range = range(c(y_range, x$ci_low_left, x$ci_high_left, x$ci_low_right, x$ci_high_right), na.rm = TRUE)
        }
        
        plot(range(c(x$x_left, x$x_right)), 
             y_range,
             type = "n", xlab = "x", ylab = "y", ...)
        
        # Draw curves/segments for each bin
        for (i in seq_len(nrow(x))) {
          if (degree >= 2 && "y_mid" %in% names(x)) {
            # Quadratic: draw smooth curve through 3 points
            x_seq = seq(x$x_left[i], x$x_right[i], length.out = 50)
            x_pts = c(x$x_left[i], x$x_mid[i], x$x_right[i])
            y_pts = c(x$y_left[i], x$y_mid[i], x$y_right[i])
            
            # Lagrange interpolation through 3 points
            y_seq = lagrange_interp_3pt(x_seq, x_pts, y_pts)
            
            # Draw CI band if requested
            if (ci && "ci_low_left" %in% names(x)) {
              # Interpolate CI bounds (approximate)
              ci_low_pts = c(x$ci_low_left[i], x$ci_low_mid[i], x$ci_low_right[i])
              ci_high_pts = c(x$ci_high_left[i], x$ci_high_mid[i], x$ci_high_right[i])
              
              ci_low_seq = lagrange_interp_3pt(x_seq, x_pts, ci_low_pts)
              ci_high_seq = lagrange_interp_3pt(x_seq, x_pts, ci_high_pts)
              
              polygon(c(x_seq, rev(x_seq)), c(ci_low_seq, rev(ci_high_seq)), 
                      col = adjustcolor("grey", alpha.f = 0.3), border = NA)
            }
            
            lines(x_seq, y_seq, ...)
          } else {
            # Linear: draw line segment
            if (ci && "ci_low_left" %in% names(x)) {
              polygon(c(x$x_left[i], x$x_right[i], x$x_right[i], x$x_left[i]),
                      c(x$ci_low_left[i], x$ci_low_right[i], x$ci_high_right[i], x$ci_high_left[i]),
                      col = adjustcolor("grey", alpha.f = 0.3), border = NA)
            }
            
            lines(c(x$x_left[i], x$x_right[i]),
                  c(x$y_left[i], x$y_right[i]), ...)
          }
        }
      }
    }
  }
  
  invisible(x)
}
