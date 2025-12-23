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
#'   Default is 0. Values greater than 0 require the \code{dbplyr} package and
#'   use constrained least squares estimation to enforce continuity at bin boundaries.
#' @param controls Optional formula specifying control variables, e.g. `~ z1 + z2`.
#'   Default is NULL (no controls).
#' @param fe Optional formula specifying fixed effects using dbreg syntax, e.g.
#'   `~ id + time`. Default is NULL (no fixed effects).
#' @param weights Character string naming the weight column. Default is NULL
#'   (equal weights).
#' @param partition_method Bin partitioning method: "quantile" (equal-count bins),
#'   "equal" (equal-width bins), "log_equal" (equal-width in log-space, for
#'   right-skewed variables; requires x > 0), or "manual" (user-specified breaks).
#'   Default is "quantile".
#' @param breaks Numeric vector of breakpoints if `partition_method = "manual"`.
#'   Ignored otherwise.
#' @param ci Logical. Calculate standard errors and confidence intervals?
#'   Default is FALSE.
#' @param vcov Character string or formula for standard errors. Options are
#'   "iid" (default if ci=TRUE), "hc1", or a clustering formula like ~cluster_var.
#'   Only supported for unconstrained estimation (smooth=0).
#' @param level Significance level for confidence intervals. Default is 0.05
#'   (95% confidence intervals). Only used when ci = TRUE.
#' @param strategy Acceleration strategy passed to dbreg when `smooth = 0`.
#'   Options are "auto" (default), "compress", or "scan". This parameter is
#'   ignored when `smooth > 0`. See \code{\link{dbreg}} for details.
#' @param conn Database connection. If NULL (default), an ephemeral DuckDB
#'   connection will be created.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A tibble with bin-level results containing:
#'   - `bin`: bin number (1 to B)
#'   - `x_left`, `x_right`, `x_mid`: bin boundaries and midpoint
#'   - `n`: number of observations in bin
#'   - If `degree = 0`: `y` (fitted value at `x_mid`)
#'   - If `degree = 1`: `y_left`, `y_right` (fitted values at bin boundaries,
#'     defining a line segment)
#'   - If `degree = 2`: `y_left`, `y_mid`, `y_right` (fitted values at boundaries
#'     and midpoint, defining a quadratic curve)
#'   - If `ci = TRUE`: `se`, `se_left`, `se_right`, `ci_low`, `ci_high` (and `_left`/`_right` variants)
#'   - Plus metadata: `B`, `degree`, `smooth`, `partition_method`
#'
#' @export
#' @examples
#' \dontrun{
#' # Simple bin means
#' dbbin(mtcars, mpg, wt, B = 10, degree = 0)
#'
#' # Piecewise linear fit
#' dbbin(mtcars, mpg, wt, B = 10, degree = 1)
#'
#' # With controls
#' dbbin(mtcars, mpg, wt, controls = ~ hp + cyl, B = 10, degree = 1)
#' }
dbbin = function(
  data,
  y,
  x,
  B = 20,
  degree = 1,
  smooth = 0,
  controls = NULL,
  fe = NULL,
  weights = NULL,
  partition_method = c("quantile", "equal", "log_equal", "manual"),
  breaks = NULL,
  ci = TRUE,
  vcov = NULL,
  level = 0.05,
  strategy = "auto",
  conn = NULL,
  verbose = TRUE
) {
  
  # Match arguments
  partition_method = match.arg(partition_method)
  
  # Validate level
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("level must be a numeric value between 0 and 1")
  }
  
  # Handle vcov / ci interaction
  if (isTRUE(ci) && is.null(vcov)) {
    vcov = "iid"
  }
  
  # Capture quoted variable names
  y_name = deparse(substitute(y))
  x_name = deparse(substitute(x))
  
  # Check for dplyr package (required for all binscatter operations)
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "The dbbin() function requires the dplyr package.\n",
      "Install it with: install.packages('dplyr')",
      call. = FALSE
    )
  }
  
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
  if (degree > 2) {
    stop("degree > 2 not supported; use degree = 0 (bin means), 1 (piecewise linear), or 2 (piecewise quadratic)")
  }
  if (partition_method == "manual" && is.null(breaks)) {
    stop("breaks must be provided when partition_method = 'manual'")
  }
  if (partition_method == "manual" && !is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) < 2) {
      stop("breaks must be a numeric vector with at least 2 values")
    }
    if (is.unsorted(breaks)) {
      stop("breaks must be sorted in increasing order")
    }
  }
  
  # Warn if user sets strategy when smooth > 0 (it will be ignored)
  if (smooth > 0 && strategy != "auto") {
    warning("'strategy' parameter is ignored when smooth > 0 (constrained estimation)", 
            call. = FALSE)
  }
  
  # Check for dbplyr when using constrained estimation
  if (smooth > 0 && !requireNamespace("dbplyr", quietly = TRUE)) {
    stop(
      "Constrained binned regression (smooth > 0) requires the dbplyr package.\n",
      "Install it with: install.packages('dbplyr')",
      call. = FALSE
    )
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
                          format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))
      dplyr::compute(data, name = table_name, temporary = TRUE)
      temp_tables = c(temp_tables, table_name)
    }
  } else if (is.data.frame(data)) {
    # Copy R dataframe to temp table
    table_name = sprintf("__db_bins_%s_input", 
                        format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))
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
  
  # Construct formula for display
  formula_str = if (!is.null(controls) && !is.null(fe)) {
    sprintf("%s ~ %s | controls + fe", y_name, x_name)
  } else if (!is.null(controls)) {
    sprintf("%s ~ %s | controls", y_name, x_name)
  } else if (!is.null(fe)) {
    sprintf("%s ~ %s | fe", y_name, x_name)
  } else {
    sprintf("%s ~ %s", y_name, x_name)
  }
  
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
    partition_method = partition_method,
    breaks = breaks,
    ci = ci,
    vcov = vcov,
    level = level,
    strategy = strategy,
    verbose = verbose,
    formula = as.formula(formula_str)
  )
  
  # Dispatch based on smooth parameter
  if (smooth == 0) {
    result = execute_unconstrained_binsreg(inputs)
  } else {
    result = execute_constrained_binsreg(inputs)
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
  partition_method = inputs$partition_method
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
  
  # Detect backend for SQL compatibility
  # SQL Server doesn't support LEAST() or LN(), so we adapt
  bd = dbreg:::detect_backend(conn)
  is_sql_server = bd$name == "sqlserver"
  
  # Backend-specific function names
  # SQL Server: LEAST() → CASE WHEN expr < val THEN expr ELSE val END
  # Others: native LEAST()
  least_fn = if (is_sql_server) {
    function(val, expr) sprintf("(CASE WHEN %s < %d THEN %s ELSE %d END)", expr, val, expr, val)
  } else {
    function(val, expr) sprintf("LEAST(%d, %s)", val, expr)
  }
  
  # SQL Server: LN() → LOG() (natural log)
  # Others: native LN()
  ln_fn = if (is_sql_server) "LOG" else "LN"
  
  # Bin assignment expression
  if (partition_method == "quantile") {
    bin_expr = sprintf("ntile(%d) OVER (ORDER BY %s)", B, x_name)
  } else if (partition_method == "equal") {
    # Equal-width bins: manually compute bin number
    # bin = 1 + floor((x - min) / width) but need to handle edge case where x = max
    bin_expr = least_fn(
      B,
      sprintf(
        "1 + FLOOR((%s - (SELECT MIN(%s) FROM %s)) / (((SELECT MAX(%s) FROM %s) - (SELECT MIN(%s) FROM %s)) / %d))",
        x_name, x_name, table_name, x_name, table_name, x_name, table_name, B
      )
    )
  } else if (partition_method == "log_equal") {
    # Equal-width bins in log-space (for right-skewed distributions)
    # Requires x > 0
    bin_expr = least_fn(
      B,
      sprintf(
        "1 + FLOOR((%s(%s) - (SELECT %s(MIN(%s)) FROM %s WHERE %s > 0)) / ((( SELECT %s(MAX(%s)) FROM %s WHERE %s > 0) - (SELECT %s(MIN(%s)) FROM %s WHERE %s > 0)) / %d))",
        ln_fn, x_name, ln_fn, x_name, table_name, x_name, ln_fn, x_name, table_name, x_name, ln_fn, x_name, table_name, x_name, B
      )
    )
  } else if (partition_method == "manual") {
    # Manual breakpoints using CASE WHEN
    # Use left-closed, right-open intervals [breaks[i], breaks[i+1])
    # except the last bin which is closed on both ends
    case_whens = character()
    for (i in 1:(length(inputs$breaks) - 1)) {
      lower = inputs$breaks[i]
      upper = inputs$breaks[i + 1]
      
      if (i == length(inputs$breaks) - 1) {
        # Last bin: closed on both ends
        case_whens = c(case_whens, 
                      sprintf("WHEN %s >= %.15g AND %s <= %.15g THEN %d", 
                             x_name, lower, x_name, upper, i))
      } else {
        # Other bins: left-closed, right-open
        case_whens = c(case_whens, 
                      sprintf("WHEN %s >= %.15g AND %s < %.15g THEN %d", 
                             x_name, lower, x_name, upper, i))
      }
    }
    
    bin_expr = sprintf("CASE %s END", paste(case_whens, collapse = " "))
  } else {
    stop("Unknown partition_method type: ", partition_method)
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
  
  # Add filter for x > 0 if using log_equal
  if (partition_method == "log_equal") {
    query = paste0(query, sprintf(" AND %s > 0", x_name))
  }
  
  # Add filter for values within breaks range if using manual
  if (partition_method == "manual") {
    min_break = min(inputs$breaks)
    max_break = max(inputs$breaks)
    query = paste0(query, sprintf(" AND %s >= %.15g AND %s <= %.15g", 
                                   x_name, min_break, x_name, max_break))
  }
  
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

#' Execute unconstrained binned regression (smooth = 0)
#' 
#' Uses dbreg() with design matrix approach for fast unconstrained estimation.
#' Leverages compress strategy when possible for efficiency with high-dimensional
#' factor variables and fixed effects.
#' 
#' @keywords internal
execute_unconstrained_binsreg = function(inputs) {
  
  if (inputs$verbose) {
    cat("[dbbin] Executing unconstrained binned regression (smooth = 0)\n")
  }
  
  # Fetch binned data into R (no temp tables created)
  binned_data = create_binned_data(inputs)
  
  # Convert bin to factor for proper dummy coding
  binned_data$bin = factor(binned_data$bin)
  
  # Compute bin geometry
  geo = compute_bin_geometry(binned_data, inputs$x_name)
  
  # Filter out bins with insufficient observations for the requested degree
  # degree 0 needs >= 1 obs
  # degree 1 needs >= 2 obs
  # degree 2 needs >= 3 obs
  min_obs = inputs$degree + 1
  insufficient_bins = geo$bin[geo$n < min_obs]
  
  if (length(insufficient_bins) > 0) {
    if (inputs$verbose) {
      cat(sprintf("[dbbin] Dropping %d bins with insufficient observations (n < %d)\n", 
                  length(insufficient_bins), min_obs))
    }
    binned_data = binned_data[!binned_data$bin %in% insufficient_bins, ]
    # Re-factor bin to drop unused levels
    binned_data$bin = droplevels(binned_data$bin)
    
    # Re-compute geometry for remaining bins (though geometry shouldn't change for remaining bins)
    geo = geo[!geo$bin %in% insufficient_bins, ]
  }
  
  # Add basis columns if needed
  if (inputs$degree > 0) {
    binned_data = add_basis_columns(binned_data, geo, inputs$x_name, inputs$degree)
    
    # Identify present bins
    present_bins = sort(as.integer(as.character(unique(binned_data$bin))))

    # Create explicit interaction columns for each bin
    # This prevents compress strategy from pooling interactions
    # Only create columns for PRESENT bins to avoid singularity
    for (bin_num in present_bins) {
      bin_val = as.character(bin_num)
      
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
    # Piecewise linear: y ~ 0 + bin + u_1 + ...
    # Only include u_i for present bins
    present_bins = sort(as.integer(as.character(unique(binned_data$bin))))
    u_terms = paste0("u_", present_bins, collapse = " + ")
    fml_rhs = paste("0 + bin", u_terms, sep = " + ")
    
  } else {
    # Piecewise quadratic
    present_bins = sort(as.integer(as.character(unique(binned_data$bin))))
    u_terms = paste0("u_", present_bins, collapse = " + ")
    u2_terms = paste0("u2_", present_bins, collapse = " + ")
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
      cat("[dbbin] Note: Using 'compress' strategy (required for binned regression)\n")
    }
    actual_strategy = "compress"
  }
  
  # Run dbreg with the R dataframe
  fit = dbreg(
    fml = fml,
    data = binned_data,
    conn = inputs$conn,
    strategy = actual_strategy,
    vcov = if (isTRUE(inputs$ci)) inputs$vcov else "iid",
    verbose = inputs$verbose
  )
  
  # Extract V_beta if available
  V_beta = if (isTRUE(inputs$ci) && !is.null(fit$vcov)) fit$vcov else NULL
  
  # Extract coefficients and construct output
  result = construct_output(inputs, fit, geo, V_beta)
  
  return(result)
}


#' Execute constrained binned regression (smooth > 0)
#' 
#' Uses moment-based estimation with KKT solver to enforce continuity constraints
#' at bin boundaries. Requires dbplyr package for SQL moment computation.
#' 
#' @keywords internal
execute_constrained_binsreg = function(inputs) {
  
  if (inputs$verbose) {
    cat("[dbbin] Executing constrained binned regression (smooth = ", inputs$smooth, ")\n", sep = "")
  }
  
  # Warn if robust/clustered SEs requested (not yet supported for constrained)
  if (isTRUE(inputs$ci) && !is.null(inputs$vcov) && !identical(inputs$vcov, "iid")) {
    warning("Robust/clustered standard errors not yet supported for constrained binscatter. Using IID standard errors.", call. = FALSE)
  }
  
  # Extract inputs
  conn = inputs$conn
  table_name = inputs$table_name
  x_name = inputs$x_name
  y_name = inputs$y_name
  B = inputs$B
  degree = inputs$degree
  smooth = inputs$smooth
  partition_method = inputs$partition_method
  weights = inputs$weights
  
  # Convert table to tbl if needed
  if (is.character(table_name)) {
    data_tbl = dplyr::tbl(conn, table_name)
  } else {
    data_tbl = table_name  # Already a tbl
  }
  
  # Step 1: Assign bins
  if (partition_method == "quantile") {
    data_binned = data_tbl %>%
      dplyr::filter(!is.na(!!rlang::sym(x_name)), !is.na(!!rlang::sym(y_name))) %>%
      dplyr::mutate(bin = dplyr::ntile(!!rlang::sym(x_name), B))
  } else if (partition_method == "equal") {
    # Equal-width bins using SQL expressions
    data_binned = data_tbl %>%
      dplyr::filter(!is.na(!!rlang::sym(x_name)), !is.na(!!rlang::sym(y_name))) %>%
      dplyr::mutate(
        bin = pmin(B, 1L + floor((!!rlang::sym(x_name) - min(!!rlang::sym(x_name), na.rm = TRUE)) / 
                                  ((max(!!rlang::sym(x_name), na.rm = TRUE) - min(!!rlang::sym(x_name), na.rm = TRUE)) / B)))
      )
  } else {
    stop("partition_method = '", partition_method, "' not yet supported for constrained estimation")
  }
  
  # Step 2: Compute bin geometry
  geo = data_binned %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      x_left = min(!!rlang::sym(x_name), na.rm = TRUE),
      x_right = max(!!rlang::sym(x_name), na.rm = TRUE),
      x_mid = mean(!!rlang::sym(x_name), na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(bin)  # IMPORTANT: sort by bin number!
  
  # Step 3: Join geometry and compute centered basis
  wt_sym = if (is.null(weights)) rlang::expr(1.0) else rlang::sym(weights)
  
  data_with_u = data_binned %>%
    dplyr::left_join(
      dplyr::copy_to(conn, geo %>% dplyr::select(bin, x_mid), 
                     name = paste0("geo_", sample.int(1e6, 1)), 
                     temporary = TRUE, overwrite = TRUE),
      by = "bin"
    ) %>%
    dplyr::mutate(
      u = !!rlang::sym(x_name) - x_mid,
      wt = !!wt_sym
    )
  
  # Add u2 if degree >= 2
  if (degree >= 2) {
    data_with_u = data_with_u %>%
      dplyr::mutate(u2 = u * u)
  }
  
  # Step 4: Compute moments per bin
  if (degree == 0) {
    moments = data_with_u %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(
        s00 = sum(wt, na.rm = TRUE),
        t0 = sum(wt * !!rlang::sym(y_name), na.rm = TRUE),
        s_yy = sum(wt * !!rlang::sym(y_name) * !!rlang::sym(y_name), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::collect()
      
  } else if (degree == 1) {
    moments = data_with_u %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(
        s00 = sum(wt, na.rm = TRUE),
        s01 = sum(wt * u, na.rm = TRUE),
        s11 = sum(wt * u * u, na.rm = TRUE),
        t0 = sum(wt * !!rlang::sym(y_name), na.rm = TRUE),
        t1 = sum(wt * !!rlang::sym(y_name) * u, na.rm = TRUE),
        s_yy = sum(wt * !!rlang::sym(y_name) * !!rlang::sym(y_name), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::collect()
      
  } else {  # degree == 2
    moments = data_with_u %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(
        s00 = sum(wt, na.rm = TRUE),
        s01 = sum(wt * u, na.rm = TRUE),
        s02 = sum(wt * u2, na.rm = TRUE),
        s11 = sum(wt * u * u, na.rm = TRUE),
        s12 = sum(wt * u * u2, na.rm = TRUE),
        s22 = sum(wt * u2 * u2, na.rm = TRUE),
        t0 = sum(wt * !!rlang::sym(y_name), na.rm = TRUE),
        t1 = sum(wt * !!rlang::sym(y_name) * u, na.rm = TRUE),
        t2 = sum(wt * !!rlang::sym(y_name) * u2, na.rm = TRUE),
        s_yy = sum(wt * !!rlang::sym(y_name) * !!rlang::sym(y_name), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::collect()
  }
  
  # Merge geometry with moments and ensure sorted by bin
  moments_full = dplyr::left_join(geo, moments, by = "bin") %>%
    dplyr::arrange(bin)
  
  # Step 5: Build constraint matrix
  A = build_constraint_matrix(moments_full, degree, smooth)
  
  # Step 6: Assemble block-diagonal X'X and X'y
  moment_system = assemble_moments(moments_full, degree)
  
  # Step 7: Solve KKT system
  kkt_sol = solve_kkt(moment_system$XtX, moment_system$Xty, A, smooth)
  beta = kkt_sol$beta
  
  # Calculate error variance
  # SSR = y'y - beta' X'y (valid for constrained OLS)
  total_s_yy = sum(moments_full$s_yy)
  ssr = total_s_yy - sum(beta * moment_system$Xty)
  
  # Degrees of freedom: N - (n_params - n_constraints)
  N = sum(moments_full$n)
  n_params = length(beta)
  n_constraints = nrow(A)
  df_resid = N - (n_params - n_constraints)
  
  sigma2 = if (df_resid > 0) ssr / df_resid else NA_real_
  
  # Covariance matrix of coefficients
  V_beta = if (!is.na(sigma2)) sigma2 * kkt_sol$V_beta_unscaled else NULL
  
  # Step 8: Construct output
  result = construct_output_from_moments(beta, moments_full, degree, smooth, partition_method, 
                                          level = inputs$level, V_beta = V_beta)
  
  return(result)
}


#' Build constraint matrix for continuity at bin boundaries
#' 
#' @param geo Tibble with bin geometry (bin, x_left, x_right, x_mid, n)
#' @param degree Polynomial degree (0, 1, or 2)
#' @param smooth Smoothness level (1 = level continuity, 2 = level + slope)
#' 
#' @return Constraint matrix A of dimension (B-1)*smooth by B*(degree+1)
#' 
#' @keywords internal
build_constraint_matrix = function(geo, degree, smooth) {
  
  B = nrow(geo)
  if (B <= 1) return(matrix(0, nrow = 0, ncol = degree + 1))
  
  n_constraints = (B - 1) * smooth
  n_params = B * (degree + 1)
  A = matrix(0, nrow = n_constraints, ncol = n_params)
  
  # For each interior boundary
  for (b in 1:(B-1)) {
    
    # Get boundary location
    x_boundary = geo$x_right[b]  # = geo$x_left[b+1]
    
    # Left polynomial: f_L(x) = beta_L0 + beta_L1*(x - x_mid_L) + beta_L2*(x - x_mid_L)^2
    x_mid_L = geo$x_mid[b]
    delta_L = x_boundary - x_mid_L
    
    # Right polynomial: f_R(x) = beta_R0 + beta_R1*(x - x_mid_R) + beta_R2*(x - x_mid_R)^2
    x_mid_R = geo$x_mid[b+1]
    delta_R = x_boundary - x_mid_R
    
    # Level continuity constraint: f_L(x_boundary) = f_R(x_boundary)
    # f_L(x_boundary) - f_R(x_boundary) = 0
    row_level = (b - 1) * smooth + 1
    col_L = (b - 1) * (degree + 1) + 1
    col_R = b * (degree + 1) + 1
    
    if (degree == 0) {
      # f_L = beta_L0, f_R = beta_R0
      A[row_level, col_L] = 1
      A[row_level, col_R] = -1
      
    } else if (degree == 1) {
      # f_L = beta_L0 + beta_L1 * delta_L
      # f_R = beta_R0 + beta_R1 * delta_R
      A[row_level, col_L] = 1
      A[row_level, col_L + 1] = delta_L
      A[row_level, col_R] = -1
      A[row_level, col_R + 1] = -delta_R
      
    } else {  # degree == 2
      # f_L = beta_L0 + beta_L1 * delta_L + beta_L2 * delta_L^2
      # f_R = beta_R0 + beta_R1 * delta_R + beta_R2 * delta_R^2
      A[row_level, col_L] = 1
      A[row_level, col_L + 1] = delta_L
      A[row_level, col_L + 2] = delta_L^2
      A[row_level, col_R] = -1
      A[row_level, col_R + 1] = -delta_R
      A[row_level, col_R + 2] = -delta_R^2
    }
    
    # Slope continuity constraint (if smooth == 2)
    # f_L'(x_boundary) = f_R'(x_boundary)
    if (smooth == 2) {
      if (degree == 0) {
        stop("Cannot enforce slope continuity (smooth=2) with constant fits (degree=0)")
      }
      
      row_slope = (b - 1) * smooth + 2
      
      if (degree == 1) {
        # f_L' = beta_L1
        # f_R' = beta_R1
        A[row_slope, col_L + 1] = 1
        A[row_slope, col_R + 1] = -1
        
      } else {  # degree == 2
        # f_L' = beta_L1 + 2 * beta_L2 * delta_L
        # f_R' = beta_R1 + 2 * beta_R2 * delta_R
        A[row_slope, col_L + 1] = 1
        A[row_slope, col_L + 2] = 2 * delta_L
        A[row_slope, col_R + 1] = -1
        A[row_slope, col_R + 2] = -2 * delta_R
      }
    }
  }
  
  return(A)
}


#' Assemble block-diagonal moment matrices
#' 
#' @param moments Tibble with columns: bin, s00, s01, s11, ... (depending on degree)
#' @param degree Polynomial degree
#' 
#' @return List with XtX (block-diagonal) and Xty (stacked vector)
#' 
#' @keywords internal
assemble_moments = function(moments, degree) {
  
  B = nrow(moments)
  p = degree + 1
  
  # Build block-diagonal X'X
  blocks = vector("list", B)
  
  if (degree == 0) {
    for (b in 1:B) {
      blocks[[b]] = matrix(moments$s00[b], 1, 1)
    }
    
  } else if (degree == 1) {
    for (b in 1:B) {
      blocks[[b]] = matrix(c(
        moments$s00[b], moments$s01[b],
        moments$s01[b], moments$s11[b]
      ), 2, 2, byrow = TRUE)
    }
    
  } else {  # degree == 2
    for (b in 1:B) {
      blocks[[b]] = matrix(c(
        moments$s00[b], moments$s01[b], moments$s02[b],
        moments$s01[b], moments$s11[b], moments$s12[b],
        moments$s02[b], moments$s12[b], moments$s22[b]
      ), 3, 3, byrow = TRUE)
    }
  }
  
  # Use Matrix package for efficient block-diagonal storage
  XtX = Matrix::bdiag(blocks)
  
  # Build X'y vector
  if (degree == 0) {
    Xty = moments$t0
  } else if (degree == 1) {
    Xty = c(rbind(moments$t0, moments$t1))
  } else {  # degree == 2
    Xty = c(t(cbind(moments$t0, moments$t1, moments$t2)))
  }
  
  return(list(XtX = XtX, Xty = Xty))
}


#' Solve constrained least squares via KKT system
#' 
#' @param XtX Block-diagonal X'X matrix (B*(degree+1) by B*(degree+1))
#' @param Xty Stacked X'y vector (length B*(degree+1))
#' @param A Constraint matrix ((B-1)*smooth by B*(degree+1))
#' @param smooth Smoothness level (for dimensionality check)
#' 
#' @return A list with components:
#' \itemize{
#'   \item \code{beta}: Beta coefficient vector (length \eqn{B \times (\mathrm{degree}+1)}).
#'   \item \code{V_beta_unscaled}: Unscaled variance-covariance matrix of \code{beta}.
#' }
#' 
#' @keywords internal
solve_kkt = function(XtX, Xty, A, smooth) {
  
  n_params = length(Xty)
  n_constraints = nrow(A)
  
  if (n_constraints == 0) {
    # No constraints, solve unconstrained
    # V_beta_unscaled = (X'X)^-1
    # Use Cholesky for stability if possible, or solve
    XtX_inv = tryCatch(
      Matrix::solve(XtX), 
      error = function(e) {
        warning("Matrix solve failed, using ridge regularization (adding 1e-10 to diagonal)", call. = FALSE)
        Matrix::solve(XtX + Matrix::Diagonal(n_params, 1e-10))
      }
    )
    beta = as.numeric(XtX_inv %*% Xty)
    return(list(beta = beta, V_beta_unscaled = XtX_inv))
  }
  
  # Build augmented KKT system:
  # [ X'X   A' ] [ beta   ]   [ X'y ]
  # [ A     0  ] [ lambda ] = [ 0   ]
  
  # Top-left: X'X
  # Top-right: A'
  # Bottom-left: A
  # Bottom-right: 0
  
  K = rbind(
    cbind(XtX, Matrix::t(A)),
    cbind(A, Matrix::Matrix(0, n_constraints, n_constraints))
  )
  
  rhs = c(Xty, rep(0, n_constraints))
  
  # Solve KKT system
  # We need the inverse of K to get V_beta
  # K_inv = [ V_beta   * ]
  #         [ *        * ]
  # The top-left block of K_inv is exactly the unscaled covariance of beta
  
  K_inv = tryCatch(
    Matrix::solve(K), 
    error = function(e) {
      warning("KKT matrix solve failed, using ridge regularization (adding 1e-10 to diagonal)", call. = FALSE)
      Matrix::solve(K + Matrix::Diagonal(nrow(K), 1e-10))
    }
  )
  solution = K_inv %*% rhs
  
  # Extract beta (first n_params elements)
  beta = as.numeric(solution[1:n_params])
  
  # Extract V_beta_unscaled (top-left block)
  V_beta_unscaled = K_inv[1:n_params, 1:n_params]
  
  return(list(beta = beta, V_beta_unscaled = V_beta_unscaled))
}


#' Construct output from constrained solution
#' 
#' @param beta Coefficient vector (length B*(degree+1))
#' @param geo Tibble with bin geometry
#' @param degree Polynomial degree
#' @param smooth Smoothness level
#' @param partition Partition type
#' @param level Significance level for CIs (default 0.05 for 95% CIs)
#' @param V_beta Covariance matrix of coefficients (optional)
#' 
#' @return Tibble with dbbin output structure
#' 
#' @keywords internal
construct_output_from_moments = function(beta, geo, degree, smooth, partition_method, level = 0.05, V_beta = NULL) {
  
  B = nrow(geo)
  p = degree + 1
  
  # Reshape beta to B x p matrix
  beta_mat = matrix(beta, nrow = B, ncol = p, byrow = TRUE)
  
  # For plotting, use the observed x_left/x_right from the data
  # The constraints are enforced at geo$x_right[b], and we evaluate 
  # both y_right[b] and y_left[b+1] at that same point to ensure continuity
  # in the output values.
  
  x_left_plot = geo$x_left
  x_right_plot = geo$x_right
  
  # Evaluate polynomials at plotting boundaries
  y_left = numeric(B)
  y_right = numeric(B)
  y_mid = if (degree >= 1) numeric(B) else rep(NA_real_, B)
  
  # Standard errors
  se_left = if (!is.null(V_beta)) numeric(B) else NULL
  se_right = if (!is.null(V_beta)) numeric(B) else NULL
  se_mid = if (!is.null(V_beta) && degree >= 1) numeric(B) else NULL
  
  for (b in 1:B) {
    x_mid_fit = geo$x_mid[b]  # The centering point for this bin's polynomial
    beta_b = beta_mat[b, ]
    
    # Indices in the full beta vector for this bin
    idx_start = (b - 1) * p + 1
    idx_end = b * p
    V_b = if (!is.null(V_beta)) V_beta[idx_start:idx_end, idx_start:idx_end] else NULL
    
    # For b > 1, evaluate y_left at the constraint boundary (x_right of previous bin)
    # This ensures y_left[b] == y_right[b-1] when constraints are satisfied
    if (b == 1) {
      delta_left = x_left_plot[b] - x_mid_fit
    } else {
      # Evaluate at constraint boundary for continuity check
      delta_left = geo$x_right[b - 1] - x_mid_fit
      # Also update x_left for this bin to match constraint point
      x_left_plot[b] = geo$x_right[b - 1]
    }
    y_left[b] = eval_poly(beta_b, delta_left, degree)
    if (!is.null(V_b)) se_left[b] = eval_se(V_b, delta_left, degree)
    
    # Right plotting boundary  
    delta_right = x_right_plot[b] - x_mid_fit
    y_right[b] = eval_poly(beta_b, delta_right, degree)
    if (!is.null(V_b)) se_right[b] = eval_se(V_b, delta_right, degree)
    
    # Midpoint of plotting region (for quadratic curves)
    if (degree >= 1) {
      x_mid_plot = (x_left_plot[b] + x_right_plot[b]) / 2
      delta_mid = x_mid_plot - x_mid_fit
      y_mid[b] = eval_poly(beta_b, delta_mid, degree)
      if (!is.null(V_b)) se_mid[b] = eval_se(V_b, delta_mid, degree)
    }
  }
  
  # Construct output tibble
  result = tibble::tibble(
    bin = geo$bin,
    x_left = x_left_plot,
    x_right = x_right_plot,
    x_mid = (x_left_plot + x_right_plot) / 2,  # Geometric midpoint for plotting
    n = as.integer(geo$n),
    y_left = y_left,
    y_mid = y_mid,
    y_right = y_right,
    B = B,
    degree = degree,
    smooth = smooth,
    partition_method = partition_method
  )
  
  # Add SEs and CIs if available
  if (!is.null(V_beta)) {
    result$se_left = se_left
    result$se_right = se_right
    if (degree >= 1) result$se_mid = se_mid
    
    # CI using normal approximation (critical value for two-sided interval)
    crit_val = stats::qnorm(1 - level / 2)
    result$ci_low_left = result$y_left - crit_val * result$se_left
    result$ci_high_left = result$y_left + crit_val * result$se_left
    result$ci_low_right = result$y_right - crit_val * result$se_right
    result$ci_high_right = result$y_right + crit_val * result$se_right
    
    if (degree >= 1) {
      result$ci_low_mid = result$y_mid - crit_val * result$se_mid
      result$ci_high_mid = result$y_mid + crit_val * result$se_mid
    }
  }
  
  # Add dbbin class
  class(result) = c("dbbin", class(result))
  
  return(result)
}


#' Evaluate polynomial at a point
#' 
#' @param beta Coefficient vector (length degree+1)
#' @param u Centered x value
#' @param degree Polynomial degree
#' 
#' @return Scalar polynomial value
#' 
#' @keywords internal
eval_poly = function(beta, u, degree) {
  if (degree == 0) {
    return(beta[1])
  } else if (degree == 1) {
    return(beta[1] + beta[2] * u)
  } else {  # degree == 2
    return(beta[1] + beta[2] * u + beta[3] * u^2)
  }
}

#' Evaluate standard error of polynomial at a point
#' 
#' @param V Covariance matrix of coefficients
#' @param u Centered x value
#' @param degree Polynomial degree
#' 
#' @return Scalar standard error
#' 
#' @keywords internal
eval_se = function(V, u, degree) {
  # x_vec = [1, u, u^2]
  if (degree == 0) {
    x_vec = matrix(1, ncol = 1)
  } else if (degree == 1) {
    x_vec = matrix(c(1, u), ncol = 1)
  } else {
    x_vec = matrix(c(1, u, u^2), ncol = 1)
  }
  
  var_pred = as.numeric(t(x_vec) %*% V %*% x_vec)
  
  if (var_pred < 0) {
    # Clamp small negative values due to numerical noise
    if (var_pred > -1e-10) {
      warning("Negative variance (", format(var_pred, scientific = TRUE), 
              ") clamped to zero due to numerical noise", call. = FALSE)
      var_pred = 0
    } else {
      warning("Large negative variance (", format(var_pred, scientific = TRUE), 
              ") indicates numerical instability, returning NA", call. = FALSE)
      return(NA_real_)
    }
  }
  
  return(sqrt(var_pred))
}


#' Construct output tibble from fitted model
#' 
#' @details
#' For degree >= 2 (quadratic), the function evaluates the piecewise polynomial
#' at three points per bin: left boundary, midpoint, and right boundary. This
#' provides sufficient information to reconstruct the quadratic curve within
#' each bin using Lagrange interpolation.
#' 
#' @keywords internal
construct_output = function(inputs, fit, geo, V_beta = NULL) {
  
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
  
  # Ensure out has B rows corresponding to bins 1..B
  # If rows are missing (empty bins), we pad with NAs
  if (nrow(out) < B) {
    # Create complete bin column matching the type of geo$bin
    if (is.factor(out$bin)) {
       # Assuming levels are correct 1..B or similar
       all_bins = factor(seq_len(B), levels = levels(out$bin))
    } else {
       all_bins = as.integer(seq_len(B))
    }
    
    template = tibble::tibble(bin = all_bins)
    out = dplyr::left_join(template, out, by = "bin")
  }
  
  out$B = B
  out$degree = degree
  out$smooth = inputs$smooth
  out$partition_method = inputs$partition_method
  
  # Helper to get SE of linear combination
  get_se = function(coef_indices, weights) {
    if (is.null(V_beta)) return(NA_real_)
    # var(w'b) = w' V w
    # We need to map coef_indices (names) to V_beta indices
    idx = match(coef_indices, rownames(V_beta))
    if (any(is.na(idx))) return(NA_real_)
    
    w = numeric(nrow(V_beta))
    w[idx] = weights
    
    var_val = as.numeric(t(w) %*% V_beta %*% w)
    if (var_val < 0) {
      if (var_val > -1e-10) var_val = 0 else return(NA_real_)
    }
    return(sqrt(var_val))
  }
  
  if (degree == 0) {
    # Extract bin means
    # With "~ 0 + bin", R creates "(Intercept)" for bin1, then "bin2", "bin3", etc.
    # Wait, with "0 + bin", R creates "bin1", "bin2", ... "binB" directly!
    # Let's check if intercept is present or not.
    # If "0 + bin" is used, we get bin1, bin2, ... binB.
    # If "bin" is used (with intercept), we get (Intercept), bin2, bin3...
    
    # dbreg uses the formula as passed. In execute_unconstrained_binsreg we use "0 + bin".
    # So we should expect "bin1", "bin2", etc.
    # BUT, if controls/FE are present, behavior might change depending on dbreg internals.
    # Let's handle both cases robustly.
    
    y_vals = numeric(B)
    se_vals = numeric(B)
    
    has_intercept = "(Intercept)" %in% coef_names
    
    for (i in 1:B) {
      bin_col = paste0("bin", i)
      
      if (!has_intercept) {
        # No intercept: coefficients are means directly
        if (bin_col %in% coef_names) {
          y_vals[i] = coef_vals[bin_col]
          se_vals[i] = get_se(bin_col, 1)
        } else {
          y_vals[i] = NA
          se_vals[i] = NA
        }
      } else {
        # With intercept: bin1 is intercept, others are deviations
        if (i == 1) {
          y_vals[i] = coef_vals["(Intercept)"]
          se_vals[i] = get_se("(Intercept)", 1)
        } else {
          if (bin_col %in% coef_names) {
            y_vals[i] = coef_vals["(Intercept)"] + coef_vals[bin_col]
            se_vals[i] = get_se(c("(Intercept)", bin_col), c(1, 1))
          } else {
            # Should not happen if bin exists
            y_vals[i] = NA
            se_vals[i] = NA
          }
        }
      }
    }
    
    out$y = y_vals
    if (!is.null(V_beta)) {
      out$se = se_vals
      # CI using normal approximation (critical value for two-sided interval)
      crit_val = stats::qnorm(1 - inputs$level / 2)
      out$ci_low = out$y - crit_val * out$se
      out$ci_high = out$y + crit_val * out$se
    }
    
  } else {
    # Piecewise polynomial: extract coefficients and evaluate at boundaries
    # With explicit u_i columns, coefficients are named: "u_1", "u_2", etc.
    
    y_left = numeric(B)
    y_mid = numeric(B)
    y_right = numeric(B)
    
    se_left = numeric(B)
    se_mid = numeric(B)
    se_right = numeric(B)
    
    has_intercept = "(Intercept)" %in% coef_names
    
    for (i in seq_len(B)) {
      # 1. Intercept (level at x_mid)
      bin_col = paste0("bin", i)
      
      if (!has_intercept) {
        # No global intercept
        b0_name = bin_col
        b0 = if (b0_name %in% coef_names) coef_vals[b0_name] else 0
        b0_idx = if (b0_name %in% coef_names) b0_name else NULL
        b0_w = if (!is.null(b0_idx)) 1 else NULL
      } else {
        # Global intercept
        if (i == 1) {
          b0 = coef_vals["(Intercept)"]
          b0_idx = "(Intercept)"
          b0_w = 1
        } else {
          b0 = coef_vals["(Intercept)"] + (if (bin_col %in% coef_names) coef_vals[bin_col] else 0)
          b0_idx = c("(Intercept)", if (bin_col %in% coef_names) bin_col else NULL)
          b0_w = c(1, if (bin_col %in% coef_names) 1 else NULL)
        }
      }
      
      # 2. Linear term (explicit columns: u_1, u_2, etc.)
      b1_name = sprintf("u_%d", i)
      b1 = if (b1_name %in% coef_names) coef_vals[b1_name] else 0
      b1_idx = if (b1_name %in% coef_names) b1_name else NULL
      
      # 3. Quadratic term if applicable
      if (degree >= 2) {
        b2_name = sprintf("u2_%d", i)
        b2 = if (b2_name %in% coef_names) coef_vals[b2_name] else 0
        b2_idx = if (b2_name %in% coef_names) b2_name else NULL
      } else {
        b2 = 0
        b2_idx = NULL
      }
      
      # Evaluate at boundaries: u_left = x_left - x_mid, u_right = x_right - x_mid
      u_left = out$x_left[i] - out$x_mid[i]
      u_right = out$x_right[i] - out$x_mid[i]
      
      # y = b0 + b1*u + b2*u^2
      y_left[i] = b0 + b1 * u_left + b2 * u_left^2
      y_mid[i] = b0  # u = 0 at midpoint
      y_right[i] = b0 + b1 * u_right + b2 * u_right^2
      
      # SEs
      if (!is.null(V_beta)) {
        # Combine indices and weights
        # Left: b0 + b1*u_left + b2*u_left^2
        idx_left = c(b0_idx, b1_idx, b2_idx)
        w_left = c(b0_w, if (!is.null(b1_idx)) u_left else NULL, if (!is.null(b2_idx)) u_left^2 else NULL)
        se_left[i] = get_se(idx_left, w_left)
        
        # Mid: b0
        se_mid[i] = get_se(b0_idx, b0_w)
        
        # Right: b0 + b1*u_right + b2*u_right^2
        idx_right = c(b0_idx, b1_idx, b2_idx)
        w_right = c(b0_w, if (!is.null(b1_idx)) u_right else NULL, if (!is.null(b2_idx)) u_right^2 else NULL)
        se_right[i] = get_se(idx_right, w_right)
      }
    }
    
    out$y_left = y_left
    out$y_mid = y_mid
    out$y_right = y_right
    
    if (!is.null(V_beta)) {
      out$se_left = se_left
      out$se_mid = se_mid
      out$se_right = se_right
      
      # CI using normal approximation (critical value for two-sided interval)
      crit_val = stats::qnorm(1 - inputs$level / 2)
      out$ci_low_left = out$y_left - crit_val * out$se_left
      out$ci_high_left = out$y_left + crit_val * out$se_left
      out$ci_low_mid = out$y_mid - crit_val * out$se_mid
      out$ci_high_mid = out$y_mid + crit_val * out$se_mid
      out$ci_low_right = out$y_right - crit_val * out$se_right
      out$ci_high_right = out$y_right + crit_val * out$se_right
    }
  }
  
  # Reorder columns sensibly
  if (degree == 0) {
    cols = c("bin", "x_left", "x_right", "x_mid", "n", "y")
    if (!is.null(V_beta)) cols = c(cols, "se", "ci_low", "ci_high")
    cols = c(cols, "B", "degree", "smooth", "partition_method")
    out = out[, cols]
  } else {
    # degree >= 1: always include y_left, y_mid, y_right
    cols = c("bin", "x_left", "x_right", "x_mid", "n", "y_left", "y_mid", "y_right")
    if (!is.null(V_beta)) {
      cols = c(cols, "se_left", "se_mid", "se_right", 
               "ci_low_left", "ci_high_left", 
               "ci_low_mid", "ci_high_mid",
               "ci_low_right", "ci_high_right")
    }
    cols = c(cols, "B", "degree", "smooth", "partition_method")
    out = out[, cols]
  }
  
  # Add S3 class and metadata attributes
  structure(
    out,
    class = c("dbbin", "tbl_df", "tbl", "data.frame"),
    fit = fit,
    formula = inputs$formula,
    breaks = inputs$breaks
  )
}


#' Lagrange interpolation through 3 points
#' 
#' @param x_seq Vector of x values to interpolate at
#' @param x_pts 3-element vector of x control points
#' @param y_pts 3-element vector of y control points
#' 
#' @return Vector of interpolated y values
#' 
#' @keywords internal
lagrange_interp_3pt = function(x_seq, x_pts, y_pts) {
  y_seq = numeric(length(x_seq))
  for (j in seq_along(x_seq)) {
    xj = x_seq[j]
    l0 = ((xj - x_pts[2]) * (xj - x_pts[3])) / ((x_pts[1] - x_pts[2]) * (x_pts[1] - x_pts[3]))
    l1 = ((xj - x_pts[1]) * (xj - x_pts[3])) / ((x_pts[2] - x_pts[1]) * (x_pts[2] - x_pts[3]))
    l2 = ((xj - x_pts[1]) * (xj - x_pts[2])) / ((x_pts[3] - x_pts[1]) * (x_pts[3] - x_pts[2]))
    y_seq[j] = y_pts[1] * l0 + y_pts[2] * l1 + y_pts[3] * l2
  }
  return(y_seq)
}


#
## S3 Methods ----
#

#' Print method for dbbin objects
#' 
#' @param x A dbbin object
#' @param ... Additional arguments passed to print
#' @export
print.dbbin = function(x, ...) {
  cat("Database binned regression\n")
  cat("Formula:", deparse(attr(x, "formula")), "\n")
  cat(sprintf("Bins: %d | Degree: %d | Partition: %s\n", 
              unique(x$B), unique(x$degree), unique(x$partition)))
  cat("\n")
  NextMethod("print")
}


#' Plot method for dbbin objects
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


#
## Moments KKT Engine (stub for future) ----
#

#' Execute bins estimation using moment-based KKT approach
#' @keywords internal
execute_bins_moments_kkt = function(inputs) {
  stop("moments_kkt engine not yet implemented")
}
