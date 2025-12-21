#' Database-native binned regression
#'
#' @description
#' Performs binned regression entirely in SQL, returning plot-ready data with
#' estimated bin means or piecewise polynomial fits. Supports unconditional and
#' conditional models (with controls and/or fixed effects).
#'
#' @param data A data source: R dataframe, database table name (character), or
#'   dplyr::tbl object pointing to a database table.
#' @param y Character string or unquoted name of the outcome variable.
#' @param x Character string or unquoted name of the binning variable.
#' @param B Integer number of bins. Default is 20.
#' @param degree Polynomial degree within bins: 0 (means), 1 (linear), or 2
#'   (quadratic). Default is 1.
#' @param smooth Smoothness at bin boundaries: 0 (discontinuous), 1 (continuous
#'   level), or 2 (continuous level and slope). Must satisfy `smooth <= degree`.
#'   Default is 0. Note: `smooth > 0` requires `engine = "moments_kkt"` (not yet
#'   implemented).
#' @param controls Optional formula specifying control variables, e.g. `~ z1 + z2`.
#'   Default is NULL (no controls).
#' @param fe Optional formula specifying fixed effects using dbreg syntax, e.g.
#'   `~ id + time`. Default is NULL (no fixed effects).
#' @param weights Character string naming the weight column. Default is NULL
#'   (equal weights).
#' @param partition Bin partitioning method: "quantile" (equal-count bins),
#'   "equal" (equal-width bins), or "manual" (user-specified breaks). Default is
#'   "quantile".
#' @param breaks Numeric vector of breakpoints if `partition = "manual"`.
#'   Ignored otherwise.
#' @param engine Estimation engine: "design_ols" (formula-based, uses existing
#'   dbreg infrastructure) or "moments_kkt" (moment-based with smoothness
#'   constraints; not yet implemented). Default is "design_ols".
#' @param strategy Acceleration strategy passed to dbreg when `engine = "design_ols"`.
#'   Default is "auto". See \code{\link{dbreg}} for details.
#' @param conn Database connection. If NULL (default), an ephemeral DuckDB
#'   connection will be created.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A tibble with bin-level results containing:
#'   - `bin`: bin number (1 to B)
#'   - `x_left`, `x_right`, `x_mid`: bin boundaries and midpoint
#'   - `n`: number of observations in bin
#'   - If `degree = 0`: `y` (fitted value at `x_mid`)
#'   - If `degree >= 1`: `y_left`, `y_right` (fitted values at bin boundaries,
#'     defining a line segment)
#'   - Plus metadata: `B`, `degree`, `smooth`, `partition`
#'
#' @export
#' @examples
#' \dontrun{
#' # Simple bin means
#' db_bins(mtcars, mpg, wt, B = 10, degree = 0)
#'
#' # Piecewise linear fit
#' db_bins(mtcars, mpg, wt, B = 10, degree = 1)
#'
#' # With controls
#' db_bins(mtcars, mpg, wt, controls = ~ hp + cyl, B = 10, degree = 1)
#' }
db_bins = function(
  data,
  y,
  x,
  B = 20,
  degree = 1,
  smooth = 0,
  controls = NULL,
  fe = NULL,
  weights = NULL,
  partition = c("quantile", "equal", "manual"),
  breaks = NULL,
  engine = c("design_ols", "moments_kkt"),
  strategy = "auto",
  conn = NULL,
  verbose = TRUE
) {
  
  # Match arguments
  partition = match.arg(partition)
  engine = match.arg(engine)
  
  # Capture quoted variable names
  y_name = deparse(substitute(y))
  x_name = deparse(substitute(x))
  
  # Validate inputs
  if (!is.numeric(B) || B < 1) {
    stop("B must be a positive integer")
  }
  if (!degree %in% 0:2) {
    stop("degree must be 0, 1, or 2")
  }
  if (!smooth %in% 0:2) {
    stop("smooth must be 0, 1, or 2")
  }
  if (smooth > degree) {
    stop("smooth must be <= degree")
  }
  if (smooth > 0 && engine == "design_ols") {
    stop("smooth > 0 requires engine = 'moments_kkt' (not yet implemented)")
  }
  if (partition == "manual" && is.null(breaks)) {
    stop("breaks must be provided when partition = 'manual'")
  }
  if (engine == "moments_kkt") {
    stop("engine = 'moments_kkt' not yet implemented; use 'design_ols'")
  }
  
  # Set up database connection
  conn_managed = FALSE
  if (is.null(conn)) {
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("Package 'duckdb' required when conn is NULL")
    }
    conn = DBI::dbConnect(duckdb::duckdb())
    conn_managed = TRUE
  }
  
  # Cleanup function
  cleanup = function() {
    if (conn_managed && DBI::dbIsValid(conn)) {
      DBI::dbDisconnect(conn, shutdown = TRUE)
    }
  }
  on.exit(cleanup(), add = TRUE)
  
  # Process data input
  temp_tables = character()
  
  if (is.character(data)) {
    # Table name provided
    table_name = data
  } else if (inherits(data, "tbl_sql")) {
    # dplyr tbl object - extract table name or use subquery
    table_name = dbplyr::remote_name(data)
    if (is.null(table_name)) {
      # Create temp table from subquery
      table_name = sprintf("__db_bins_%s_input", 
                          paste0(sample(letters, 8, replace = TRUE), collapse = ""))
      dplyr::compute(data, name = table_name, temporary = TRUE)
      temp_tables = c(temp_tables, table_name)
    }
  } else if (is.data.frame(data)) {
    # Copy R dataframe to temp table
    table_name = sprintf("__db_bins_%s_input", 
                        paste0(sample(letters, 8, replace = TRUE), collapse = ""))
    DBI::dbWriteTable(conn, table_name, data, temporary = TRUE)
    temp_tables = c(temp_tables, table_name)
  } else {
    stop("data must be a dataframe, table name, or dplyr::tbl object")
  }
  
  # Cleanup temp tables
  cleanup_tables = function() {
    if (length(temp_tables) > 0 && DBI::dbIsValid(conn)) {
      for (tbl in temp_tables) {
        try(DBI::dbRemoveTable(conn, tbl, fail_if_missing = FALSE), silent = TRUE)
      }
    }
  }
  on.exit(cleanup_tables(), add = TRUE)
  
  # Bundle inputs
  inputs = list(
    conn = conn,
    table_name = table_name,
    y_name = y_name,
    x_name = x_name,
    B = as.integer(B),
    degree = degree,
    smooth = smooth,
    controls = controls,
    fe = fe,
    weights = weights,
    partition = partition,
    breaks = breaks,
    engine = engine,
    strategy = strategy,
    verbose = verbose
  )
  
  # Dispatch to engine
  if (engine == "design_ols") {
    result = execute_bins_design_ols(inputs)
  } else {
    result = execute_bins_moments_kkt(inputs)
  }
  
  return(result)
}


#
## SQL Infrastructure Helpers ----
#

#' Build bin assignment CTE and fetch binned data into R
#' @keywords internal
create_binned_data = function(inputs) {
  
  conn = inputs$conn
  table_name = inputs$table_name
  x_name = inputs$x_name
  y_name = inputs$y_name
  B = inputs$B
  partition = inputs$partition
  weights = inputs$weights
  controls = inputs$controls
  fe = inputs$fe
  
  # Build column list
  cols = c(y_name, x_name)
  if (!is.null(weights)) cols = c(cols, weights)
  if (!is.null(controls)) {
    control_vars = all.vars(controls)
    cols = c(cols, control_vars)
  }
  if (!is.null(fe)) {
    fe_vars = all.vars(fe)
    cols = c(cols, fe_vars)
  }
  
  # Weight column handling
  wt_expr = if (is.null(weights)) "1.0" else weights
  
  # Bin assignment expression
  if (partition == "quantile") {
    bin_expr = sprintf("ntile(%d) OVER (ORDER BY %s)", B, x_name)
  } else if (partition == "equal") {
    bin_expr = sprintf(
      "width_bucket(%s, (SELECT MIN(%s) FROM %s), (SELECT MAX(%s) FROM %s), %d)",
      x_name, x_name, table_name, x_name, table_name, B
    )
  } else {
    # manual - will implement if needed
    stop("partition = 'manual' not yet implemented")
  }
  
  # Build query - fetch binned data into R
  col_list = paste(cols, collapse = ", ")
  
  query = sprintf(
    "SELECT
      %s,
      %s AS __db_bins_wt,
      %s AS bin
    FROM %s
    WHERE %s IS NOT NULL AND %s IS NOT NULL",
    col_list,
    wt_expr,
    bin_expr,
    table_name,
    y_name,
    x_name
  )
  
  # Add filter for non-null controls/fe if present
  if (!is.null(controls)) {
    for (v in all.vars(controls)) {
      query = paste0(query, sprintf(" AND %s IS NOT NULL", v))
    }
  }
  if (!is.null(fe)) {
    for (v in all.vars(fe)) {
      query = paste0(query, sprintf(" AND %s IS NOT NULL", v))
    }
  }
  
  # Fetch into R
  binned_data = DBI::dbGetQuery(conn, query)
  
  return(binned_data)
}


#' Compute bin geometry from binned data
#' @keywords internal
compute_bin_geometry = function(binned_data, x_name) {
  
  # Group by bin and compute geometry
  geo = stats::aggregate(
    binned_data[[x_name]],
    by = list(bin = binned_data$bin),
    FUN = function(x) {
      c(x_left = min(x), x_right = max(x), x_mid = mean(x), n = length(x))
    }
  )
  
  # Unpack the matrix column
  geo_mat = geo$x
  geo = data.frame(
    bin = geo$bin,
    x_left = geo_mat[, "x_left"],
    x_right = geo_mat[, "x_right"],
    x_mid = geo_mat[, "x_mid"],
    n = geo_mat[, "n"]
  )
  
  return(geo)
}


#' Add centered polynomial basis to binned data
#' @keywords internal
add_basis_columns = function(binned_data, geo, x_name, degree) {
  
  if (degree == 0) {
    return(binned_data)  # No basis needed for means
  }
  
  # Merge in x_mid
  binned_data = merge(binned_data, geo[, c("bin", "x_mid")], by = "bin")
  
  # Add centered polynomial terms
  binned_data$u = binned_data[[x_name]] - binned_data$x_mid
  
  if (degree >= 2) {
    binned_data$u2 = binned_data$u^2
  }
  
  return(binned_data)
}


#
## Design OLS Engine ----
#

#' Execute bins estimation using design matrix / OLS approach
#' @keywords internal
execute_bins_design_ols = function(inputs) {
  
  if (inputs$verbose) {
    cat("[db_bins] Executing design_ols strategy\n")
  }
  
  # Fetch binned data into R (no temp tables created)
  binned_data = create_binned_data(inputs)
  
  # Convert bin to factor for proper dummy coding
  binned_data$bin = factor(binned_data$bin)
  
  # Compute bin geometry
  geo = compute_bin_geometry(binned_data, inputs$x_name)
  
  # Add basis columns if needed
  if (inputs$degree > 0) {
    binned_data = add_basis_columns(binned_data, geo, inputs$x_name, inputs$degree)
    
    # Create explicit interaction columns for each bin
    # This prevents compress strategy from pooling interactions
    for (bin_val in levels(binned_data$bin)) {
      bin_num = as.integer(bin_val)
      # Create bin-specific u columns: u_1, u_2, etc.
      col_name = paste0("u_", bin_num)
      binned_data[[col_name]] = ifelse(binned_data$bin == bin_val, binned_data$u, 0)
      
      if (inputs$degree >= 2) {
        col_name2 = paste0("u2_", bin_num)
        binned_data[[col_name2]] = ifelse(binned_data$bin == bin_val, binned_data$u2, 0)
      }
    }
  }
  
  # Build formula based on degree
  y_name = inputs$y_name
  degree = inputs$degree
  controls = inputs$controls
  fe = inputs$fe
  
  if (degree == 0) {
    # Bin means: y ~ 0 + bin
    fml_rhs = "0 + bin"
    
  } else if (degree == 1) {
    # Piecewise linear: y ~ 0 + bin + u_1 + u_2 + ... + u_B
    u_terms = paste0("u_", seq_len(inputs$B), collapse = " + ")
    fml_rhs = paste("0 + bin", u_terms, sep = " + ")
    
  } else {
    # Piecewise quadratic
    u_terms = paste0("u_", seq_len(inputs$B), collapse = " + ")
    u2_terms = paste0("u2_", seq_len(inputs$B), collapse = " + ")
    fml_rhs = paste("0 + bin", u_terms, u2_terms, sep = " + ")
  }
  
  # Add controls if present
  if (!is.null(controls)) {
    control_terms = paste(all.vars(controls), collapse = " + ")
    fml_rhs = paste(fml_rhs, control_terms, sep = " + ")
  }
  
  # Build full formula
  if (!is.null(fe)) {
    fe_terms = paste(all.vars(fe), collapse = " + ")
    fml_str = sprintf("%s ~ %s | %s", y_name, fml_rhs, fe_terms)
  } else {
    fml_str = sprintf("%s ~ %s", y_name, fml_rhs)
  }
  
  fml = stats::as.formula(fml_str)
  
  # Force compress strategy for factor bins (moments doesn't support factors)
  actual_strategy = if (inputs$strategy == "auto") "compress" else inputs$strategy
  if (actual_strategy %in% c("moments", "demean", "within", "mundlak")) {
    if (inputs$verbose) {
      cat("[db_bins] Note: Using 'compress' strategy (required for binned regression)\n")
    }
    actual_strategy = "compress"
  }
  
  # Run dbreg with the R dataframe
  fit = dbreg(
    fml = fml,
    data = binned_data,
    conn = inputs$conn,
    strategy = actual_strategy,
    vcov = "iid",
    verbose = inputs$verbose
  )
  
  # Extract coefficients and construct output
  result = construct_output(inputs, fit, geo)
  
  return(result)
}


#' Construct output tibble from fitted model
#' @keywords internal
construct_output = function(inputs, fit, geo) {
  
  degree = inputs$degree
  B = inputs$B
  
  # Extract coefficients from coeftable (coef() returns NULL in dbreg)
  if (is.null(fit$coeftable)) {
    stop("No coefficients found in fit object")
  }
  
  coef_names = rownames(fit$coeftable)
  coef_vals = fit$coeftable[, "estimate"]
  names(coef_vals) = coef_names
  
  # Start with geometry
  out = tibble::as_tibble(geo)
  out$B = B
  out$degree = degree
  out$smooth = inputs$smooth
  out$partition = inputs$partition
  
  if (degree == 0) {
    # Extract bin means
    # With "~ 0 + bin", R creates "(Intercept)" for bin1, then "bin2", "bin3", etc.
    
    y_vals = numeric(B)
    
    # Bin 1 is the "(Intercept)"
    if ("(Intercept)" %in% coef_names) {
      y_vals[1] = coef_vals["(Intercept)"]
    } else {
      y_vals[1] = NA
    }
    
    # Bins 2-B are deviations from bin1
    for (i in 2:B) {
      coef_name = sprintf("bin%d", i)
      if (coef_name %in% coef_names) {
        # This is a deviation from the intercept, so add them
        y_vals[i] = y_vals[1] + coef_vals[coef_name]
      } else {
        y_vals[i] = NA
      }
    }
    
    out$y = y_vals
    
  } else {
    # Piecewise polynomial: extract coefficients and evaluate at boundaries
    # With explicit u_i columns, coefficients are named: "u_1", "u_2", etc.
    
    y_left = numeric(B)
    y_right = numeric(B)
    
    for (i in seq_len(B)) {
      # Intercept (level at x_mid)
      if (i == 1) {
        b0_name = "(Intercept)"
        b0 = if (b0_name %in% coef_names) coef_vals[b0_name] else 0
      } else {
        # Bins 2+ are deviations from bin1
        b0_name = sprintf("bin%d", i)
        intercept_val = if ("(Intercept)" %in% coef_names) coef_vals["(Intercept)"] else 0
        deviation = if (b0_name %in% coef_names) coef_vals[b0_name] else 0
        b0 = intercept_val + deviation
      }
      
      # Linear term (explicit columns: u_1, u_2, etc.)
      b1_name = sprintf("u_%d", i)
      b1 = if (b1_name %in% coef_names) coef_vals[b1_name] else 0
      
      # Quadratic term if applicable
      if (degree >= 2) {
        b2_name = sprintf("u2_%d", i)
        b2 = if (b2_name %in% coef_names) coef_vals[b2_name] else 0
      } else {
        b2 = 0
      }
      
      # Evaluate at boundaries: u_left = x_left - x_mid, u_right = x_right - x_mid
      u_left = geo$x_left[i] - geo$x_mid[i]
      u_right = geo$x_right[i] - geo$x_mid[i]
      
      y_left[i] = b0 + b1 * u_left + b2 * u_left^2
      y_right[i] = b0 + b1 * u_right + b2 * u_right^2
    }
    
    out$y_left = y_left
    out$y_right = y_right
  }
  
  # Reorder columns sensibly
  if (degree == 0) {
    out = out[, c("bin", "x_left", "x_right", "x_mid", "n", "y", 
                 "B", "degree", "smooth", "partition")]
  } else {
    out = out[, c("bin", "x_left", "x_right", "x_mid", "n", 
                 "y_left", "y_right", "B", "degree", "smooth", "partition")]
  }
  
  return(out)
}


#
## Moments KKT Engine (stub for future) ----
#

#' Execute bins estimation using moment-based KKT approach
#' @keywords internal
execute_bins_moments_kkt = function(inputs) {
  stop("moments_kkt engine not yet implemented")
}
