#' Database-native binned regression
#'
#' @md
#' @description Performs binned regression entirely in SQL, returning plot-ready data with
#' estimated bin means or piecewise polynomial fits. Supports unconditional and
#' conditional models (with controls and/or fixed effects).
#'
#' @param fml A \code{\link[stats]{formula}} representing the binscatter relation.
#'   The first variable on the RHS is the running variable; additional variables
#'   are controls. Fixed effects go after `|`. Examples:
#'   - `y ~ x`: simple binscatter
#'   - `y ~ x + w1 + w2`: binscatter with controls
#'   - `y ~ x | fe`: binscatter with fixed effects
#'   - `y ~ x + w1 + w2 | fe`: binscatter with controls and fixed effects
#' @param data A data source: R dataframe, database table name (character), or
#'   dplyr::tbl object pointing to a database table.
#' @param B Integer number of bins. Default is 20.
#' @param degree Polynomial degree within bins: 0 (means), 1 (linear), or 2
#'   (quadratic). Default is 1.
#' @param smooth Smoothness at bin boundaries: 0 (discontinuous), 1 (continuous
#'   level), or 2 (continuous level and slope). Must satisfy `smooth <= degree`.
#'   Default is 0. Values greater than 0 use a regression spline (truncated-power
#'   basis) that automatically enforces continuity constraints. Controls and fixed
#'   effects are supported with all smooth values.
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
#' @param level Significance level for confidence intervals. Default is 0.05
#'   (95% confidence intervals). Only used when ci = TRUE.
#' @param strategy Acceleration strategy passed to dbreg when `smooth = 0`.
#'   Options are "auto" (default), "compress", or "scan". This parameter is
#'   ignored when `smooth > 0`. See \code{\link{dbreg}} for details.
#' @param conn Database connection. If NULL (default), an ephemeral DuckDB
#'   connection will be created.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A data frame with bin-level results containing:
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
#' dbbin(mpg ~ wt, mtcars, B = 10, degree = 0)
#'
#' # Piecewise linear fit
#' dbbin(mpg ~ wt, mtcars, B = 10, degree = 1)
#'
#' # With controls
#' dbbin(mpg ~ wt + hp + cyl, mtcars, B = 10, degree = 1)
#'
#' # With fixed effects
#' dbbin(mpg ~ wt | gear, mtcars, B = 10, degree = 1)
#'
#' # With controls and fixed effects
#' dbbin(mpg ~ wt + hp + cyl | gear, mtcars, B = 10, degree = 1)
#'
#' # Constrained (continuous) binscatter with controls
#' dbbin(mpg ~ wt + hp | gear, mtcars, B = 10, degree = 1, smooth = 1)
#' }
dbbin = function(
  fml,
  data,
  B = 20,
  degree = 1,
  smooth = 0,
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
  
  # Parse formula using Formula package (same pattern as dbreg)
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop(
      "The dbbin() function requires the Formula package.\n",
      "Install it with: install.packages('Formula')",
      call. = FALSE
    )
  }
  
  fml = Formula::Formula(fml)
  
  # Extract y variable (LHS)
  y_name = all.vars(formula(fml, lhs = 1, rhs = 0))
  if (length(y_name) != 1) {
    stop("Exactly one outcome variable required on LHS of formula")
  }
  
  # Extract RHS part 1: x variable (first) and controls (rest)
  # Formula: y ~ x + controls | fe
  rhs1_vars = all.vars(formula(fml, lhs = 0, rhs = 1))
  if (length(rhs1_vars) < 1) {
    stop("At least one variable (the running variable) required on RHS of formula")
  }
  
  x_name = rhs1_vars[1]  # First variable is the running variable
  controls = if (length(rhs1_vars) > 1) rhs1_vars[-1] else NULL
  
  # Extract fixed effects (second RHS component after |, if present)
  fe = if (length(fml)[2] > 1) {
    all.vars(formula(fml, lhs = 0, rhs = 2))
  } else {
    NULL
  }
  
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
                          gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
      dplyr::compute(data, name = table_name, temporary = TRUE)
      temp_tables = c(temp_tables, table_name)
    }
  } else if (is.data.frame(data)) {
    # Copy R dataframe to temp table
    table_name = sprintf("__db_bins_%s_input", 
                        gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
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
    sprintf("%s ~ %s | %s | %s", y_name, x_name, paste(controls, collapse = " + "), paste(fe, collapse = " + "))
  } else if (!is.null(controls)) {
    sprintf("%s ~ %s | %s", y_name, x_name, paste(controls, collapse = " + "))
  } else if (!is.null(fe)) {
    sprintf("%s ~ %s | %s", y_name, x_name, paste(fe, collapse = " + "))
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
    cols = c(cols, controls)
  }
  if (!is.null(fe)) {
    cols = c(cols, fe)
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
    for (v in controls) {
      query = paste0(query, sprintf(" AND %s IS NOT NULL", v))
    }
  }
  if (!is.null(fe)) {
    for (v in fe) {
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
    control_terms = paste(controls, collapse = " + ")
    fml_rhs = paste(fml_rhs, control_terms, sep = " + ")
  }
  
  # Build full formula
  if (!is.null(fe)) {
    fe_terms = paste(fe, collapse = " + ")
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
    cat("[dbbin] Executing constrained binscatter via regression splines (smooth = ", 
        inputs$smooth, ")\n", sep = "")
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
  controls = inputs$controls
  fe = inputs$fe
  vcov_type = if (isTRUE(inputs$ci)) inputs$vcov else "iid"
  
  # Convert table to tbl if needed
  if (is.character(table_name)) {
    data_tbl = dplyr::tbl(conn, table_name)
  } else {
    data_tbl = table_name
  }
  
  # Step 1: Assign bins and compute geometry
  if (partition_method == "quantile") {
    data_binned = data_tbl |>
      dplyr::filter(!is.na(!!as.symbol(x_name)), !is.na(!!as.symbol(y_name))) |>
      dplyr::mutate(bin = dplyr::ntile(!!as.symbol(x_name), B))
  } else if (partition_method == "equal") {
    # Note: Using if_else below instead of pmin for SQL Server compatibility (no LEAST function)
    data_binned = data_tbl |>
      dplyr::filter(!is.na(!!as.symbol(x_name)), !is.na(!!as.symbol(y_name))) |>
      dplyr::mutate(
        raw_bin = 1L + floor((!!as.symbol(x_name) - min(!!as.symbol(x_name), na.rm = TRUE)) / 
                             ((max(!!as.symbol(x_name), na.rm = TRUE) - min(!!as.symbol(x_name), na.rm = TRUE)) / B))
      ) |>
      dplyr::mutate(
        bin = dplyr::if_else(raw_bin > !!B, !!B, raw_bin)
      ) |>
      dplyr::select(-raw_bin)
  } else {
    stop("partition_method = '", partition_method, "' not supported")
  }
  
  # Compute bin geometry (boundaries and counts)
  geo = data_binned |>
    dplyr::group_by(bin) |>
    dplyr::summarise(
      x_left = min(!!as.symbol(x_name), na.rm = TRUE),
      x_right = max(!!as.symbol(x_name), na.rm = TRUE),
      x_mid = mean(!!as.symbol(x_name), na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::arrange(bin)
  
  # Step 2: Extract interior knots from bin boundaries
  # Knots are at x_right[1], x_right[2], ..., x_right[B-1]
  knots = geo$x_right[1:(B-1)]
  
  if (inputs$verbose) {
    cat("[dbbin] Using ", length(knots), " interior knots for spline basis\n", sep = "")
  }
  
  # Step 3: Build spline basis columns IN SQL (avoid collecting to R!)
  # Use dplyr::mutate to add columns in the database
  x_sym = as.symbol(x_name)
  basis_names = character()
  
  # Global polynomial part (excluding intercept, which dbreg handles)
  if (degree >= 1) {
    data_binned = data_binned |> dplyr::mutate(x_spline = !!x_sym)
    basis_names = c(basis_names, "x_spline")
  }
  if (degree >= 2) {
    data_binned = data_binned |> dplyr::mutate(x2_spline = (!!x_sym) * (!!x_sym))
    basis_names = c(basis_names, "x2_spline")
  }
  
  # Truncated power terms at each knot: (x - κ_j)₊^r for r = smooth, ..., degree
  for (j in seq_along(knots)) {
    kappa = knots[j]
    for (r in smooth:degree) {
      col_name = sprintf("knot%d_pow%d", j, r)
      
      if (r == 1) {
        # (x - κ)₊ = CASE WHEN x > κ THEN x - κ ELSE 0 END
        data_binned = data_binned |>
          dplyr::mutate(
            !!col_name := dplyr::if_else(!!x_sym > !!kappa, !!x_sym - !!kappa, 0)
          )
      } else if (r == 2) {
        # (x - κ)₊² = CASE WHEN x > κ THEN (x - κ)² ELSE 0 END
        data_binned = data_binned |>
          dplyr::mutate(
            !!col_name := dplyr::if_else(!!x_sym > !!kappa, (!!x_sym - !!kappa) * (!!x_sym - !!kappa), 0)
          )
      }
      basis_names = c(basis_names, col_name)
    }
  }
  
  # Step 4: Build formula for dbreg
  # y ~ basis_terms [+ controls] [| fe]
  rhs_terms = basis_names
  if (!is.null(controls) && length(controls) > 0) {
    rhs_terms = c(rhs_terms, controls)
  }
  rhs = paste(rhs_terms, collapse = " + ")
  
  if (!is.null(fe) && length(fe) > 0) {
    fe_part = paste(fe, collapse = " + ")
    fml_str = sprintf("%s ~ %s | %s", y_name, rhs, fe_part)
  } else {
    fml_str = sprintf("%s ~ %s", y_name, rhs)
  }
  fml = stats::as.formula(fml_str)
  
  if (inputs$verbose) {
    cat("[dbbin] Fitting regression spline \n")
  }
  
  # Step 5: Run regression via dbreg
  # Pass the tbl as `table` argument (not `data`) - dbreg accepts tbl_lazy
  # Note: weights not yet supported by dbreg, warn if specified
  if (!is.null(weights)) {
    warning("Weights not yet supported for constrained binscatter; ignoring weights argument", call. = FALSE)
  }
  
  fit = dbreg(
    fml = fml,
    table = data_binned,  # Pass tbl_lazy via table argument
    conn = conn,
    vcov = vcov_type,
    verbose = FALSE
  )
  
  # Step 6: Evaluate fitted spline at bin endpoints
  result = evaluate_spline_at_bins(fit, geo, knots, degree, smooth, basis_names, inputs)
  
  return(result)
}


#' Extract interior knots from bin geometry
#' 
#' @param geo Data frame with bin geometry (must have x_right column)
#' @return Numeric vector of K = B-1 interior knots
#' @keywords internal
extract_knots = function(geo) {
  B = nrow(geo)
  if (B <= 1) return(numeric(0))
  geo$x_right[1:(B-1)]
}


#' Evaluate fitted spline at bin boundaries
#' 
#' @param fit dbreg fit object
#' @param geo Bin geometry data frame
#' @param knots Numeric vector of knot locations
#' @param degree Polynomial degree
#' @param smooth Smoothness level
#' @param basis_names Character vector of basis column names
#' @param inputs Original inputs list (for metadata)
#' 
#' @return Data frame with bin-level fitted values
#' @keywords internal
evaluate_spline_at_bins = function(fit, geo, knots, degree, smooth, basis_names, inputs) {
  
  B = nrow(geo)
  
  # Extract coefficients
  coef_tbl = fit$coeftable
  coefs = stats::setNames(coef_tbl[, "estimate"], rownames(coef_tbl))
  
  # Get intercept
  intercept = if ("(Intercept)" %in% names(coefs)) coefs["(Intercept)"] else 0
  
  # Get variance-covariance matrix if CI requested
  V = if (isTRUE(inputs$ci)) stats::vcov(fit) else NULL
  
  # Function to build basis vector at a given x value
  build_basis_vector = function(x_val) {
    bvec = numeric(length(basis_names))
    names(bvec) = basis_names
    
    # Global polynomial part
    if (degree >= 1 && "x_spline" %in% basis_names) {
      bvec["x_spline"] = x_val
    }
    if (degree >= 2 && "x2_spline" %in% basis_names) {
      bvec["x2_spline"] = x_val^2
    }
    
    # Knot features
    for (j in seq_along(knots)) {
      for (r in smooth:degree) {
        col_name = sprintf("knot%d_pow%d", j, r)
        if (col_name %in% basis_names) {
          bvec[col_name] = pmax(0, x_val - knots[j])^r
        }
      }
    }
    bvec
  }
  
  # Function to evaluate spline: ŷ(x) = intercept + b(x)' β
  eval_spline = function(x_val) {
    bvec = build_basis_vector(x_val)
    beta_basis = coefs[basis_names]
    # Handle any missing coefficients (shouldn't happen, but be safe)
    beta_basis[is.na(beta_basis)] = 0
    intercept + sum(bvec * beta_basis)
  }
  
  # Function to compute SE: sqrt(b(x)' V b(x))
  eval_se = function(x_val) {
    if (is.null(V)) return(NA_real_)
    
    # Build full basis vector including intercept
    bvec_full = c(1, build_basis_vector(x_val))
    names(bvec_full)[1] = "(Intercept)"
    
    # Match to V dimensions
    v_names = rownames(V)
    bvec_matched = rep(0, length(v_names))
    names(bvec_matched) = v_names
    
    for (nm in names(bvec_full)) {
      if (nm %in% v_names) {
        bvec_matched[nm] = bvec_full[nm]
      }
    }
    
    var_pred = as.numeric(t(bvec_matched) %*% V %*% bvec_matched)
    if (var_pred < 0) {
      if (var_pred > -1e-10) var_pred = 0 else return(NA_real_)
    }
    sqrt(var_pred)
  }
  
  # Evaluate at bin endpoints
  if (degree == 0) {
    # For degree 0, evaluate at midpoints
    geo$y = sapply(geo$x_mid, eval_spline)
    if (!is.null(V)) {
      geo$se = sapply(geo$x_mid, eval_se)
      crit = stats::qnorm(1 - inputs$level / 2)
      geo$ci_low = geo$y - crit * geo$se
      geo$ci_high = geo$y + crit * geo$se
    }
  } else {
    # For degree >= 1, evaluate at left, mid, right
    geo$y_left = sapply(geo$x_left, eval_spline)
    geo$y_mid = sapply(geo$x_mid, eval_spline)
    geo$y_right = sapply(geo$x_right, eval_spline)
    
    if (!is.null(V)) {
      geo$se_left = sapply(geo$x_left, eval_se)
      geo$se_mid = sapply(geo$x_mid, eval_se)
      geo$se_right = sapply(geo$x_right, eval_se)
      
      crit = stats::qnorm(1 - inputs$level / 2)
      geo$ci_low_left = geo$y_left - crit * geo$se_left
      geo$ci_high_left = geo$y_left + crit * geo$se_left
      geo$ci_low_mid = geo$y_mid - crit * geo$se_mid
      geo$ci_high_mid = geo$y_mid + crit * geo$se_mid
      geo$ci_low_right = geo$y_right - crit * geo$se_right
      geo$ci_high_right = geo$y_right + crit * geo$se_right
    }
  }
  
  # Add metadata
  geo$B = B
  geo$degree = degree
  geo$smooth = smooth
  geo$partition_method = inputs$partition_method
  
  # Add dbbin class and attributes
  structure(
    geo,
    class = c("dbbin", class(geo)),
    fit = fit,
    formula = inputs$formula,
    knots = knots
  )
}


#' Construct output from constrained solution
#' 
#' @md
#' @description Internal helper that transforms coefficient estimates from constrained least
#' squares into a structured data frame with bin-level fitted values, standard errors,
#' and confidence intervals.
#' 
#' Construct output data frame from fitted model
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
  
  # Start with geometry (already a data frame from dplyr::collect())
  out = geo
  
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
    
    template = data.frame(bin = all_bins)
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