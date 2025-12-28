#' Database-native binned regression (binsreg-compatible API)
#'
#' @md
#' @description Performs binned regression entirely in SQL, returning plot-ready data with
#' estimated bin means or piecewise polynomial fits. The API is designed to be compatible
#' with the \pkg{binsreg} package by Cattaneo, Crump, Farrell, and Feng (2024).
#' Supports unconditional and conditional models (with controls and/or fixed effects).
#'
#' @param fml A \code{\link[stats]{formula}} representing the binscatter relation.
#'   The first variable on the RHS is the running variable; additional variables
#'   are controls. Fixed effects go after `|`. Examples:
#'   - `y ~ x`: simple binscatter
#'   - `y ~ x + w1 + w2`: binscatter with controls
#'   - `y ~ x | fe`: binscatter with fixed effects
#'   - `y ~ x + w1 + w2 | fe`: binscatter with controls and fixed effects
#' @param data A data source: R dataframe, database table name (character), or
#'   a lazy table object (e.g., from `dplyr::tbl()`) pointing to a database table.
#' @param dots A vector `c(p, s)` specifying the polynomial degree `p` and smoothness
#'   `s` for the dots (point estimates at bin means). Default is `c(0, 0)` for
#'   canonical binscatter (bin means). Set to `NULL` or `FALSE` to suppress dots.
#'   The smoothness `s` must satisfy `s <= p`.
#' @param line A vector `c(p, s)` specifying the polynomial degree `p` and smoothness
#'   `s` for the line (evaluated on a grid within bins). Default is `NULL` (no line).
#'   Set to `TRUE` for `c(0, 0)` or a vector like `c(1, 1)` for piecewise linear
#'   with continuity constraints. The smoothness `s` must satisfy `s <= p`.
#' @param linegrid Number of evaluation points per bin for the line. Default is 20.
#' @param nbins Integer number of bins. Default is 20.
#' @param binspos Bin positioning method: "qs" (quantile-spaced, equal-count bins,
#'   the default), "es" (evenly-spaced, equal-width bins), or a numeric vector of
#'   knot positions for manual specification.
#' @param sample_frac Numeric between 0 and 1, or NULL (default). Controls
#'   sampling for bin boundary computation on large datasets. If NULL, sampling
#'   is automatic: 10% for datasets exceeding 1 million rows, 100% otherwise.
#'   Set explicitly to override (e.g., 1 to always use all data, 0.05 for 5%).
#'   Sampling only affects bin boundary computation; the regression uses all data.
#' @param ci Logical. Calculate standard errors and confidence intervals for dots?
#'   Default is TRUE.
#' @param vcov Character string or formula for standard errors. Options are
#'   "HC1" (default, heteroskedasticity-robust, matches binsreg), "iid", or a
#'   clustering formula like ~cluster_var.
#' @param level Confidence level as a percentage (e.g., 95 for 95% CI). Default is 95.
#' @param strategy Acceleration strategy passed to dbreg when smoothness is 0.
#'   Options are "auto" (default), "compress", or "scan". This parameter is
#'   ignored when `s` (smoothness parameter in `dots` or `lines`) > 0. See \code{\link{dbreg}} for details.
#' @param conn Database connection. If NULL (default), an ephemeral DuckDB
#'   connection will be created.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A list of class "dbbin" containing:
#' \describe{
#'   \item{data.dots}{Data frame with dot estimates (one row per bin): `x` (bin mean),
#'     `bin`, `fit` (fitted value), and if `ci=TRUE`: `se`, `ci.l`, `ci.r`.}
#'   \item{data.line}{Data frame with line estimates (multiple rows per bin): `x`,
#'     `bin`, `fit`. Only present if `line` is specified.}
#'   \item{data.bin}{Data frame with bin geometry: `bin.id`, `left.endpoint`,
#'     `right.endpoint`.}
#'   \item{model}{The fitted dbreg model object (for dots).}
#'   \item{opt}{List of options used: `dots`, `line`, `nbins`, `binspos`, etc.}
#' }
#'
#' @details
#' ## Relationship to binsreg
#'
#' This function aims to provide an API similar to the \pkg{binsreg} package.
#' Key parameter mappings:
#' \itemize{
#'   \item `dots=c(0,0)`: Canonical binscatter (bin means), equivalent to binsreg default
#'   \item `dots=c(p,0)`: Piecewise polynomial of degree p, no smoothness constraints
#'   \item `dots=c(p,s)`: Piecewise polynomial with s smoothness constraints at knots
#'   \item `line=c(p,s)`: Same polynomial evaluated on a grid for smooth visualization
#'   \item `binspos="qs"`: Quantile-spaced bins (binsreg default)
#'   \item `binspos="es"`: Evenly-spaced bins
#' }
#'
#' Unlike binsreg, dbbin executes entirely in SQL, making it suitable for large
#' databases that cannot fit in memory.
#'
#' ## Note on quantile bin boundaries
#'
#' When using quantile-spaced bins (`binspos="qs"`), dbbin uses SQL's `NTILE()`
#' window function, while binsreg uses R's `quantile()` with
#' `type=2`. These algorithms have slightly different tie-breaking behavior,
#' which can cause small differences in bin assignments at boundaries. In
#' practice, differences are typically <1% and become negligible with larger
#' datasets. To match binsreg exactly, compute quantile breaks on a subset 
#' of data in R and pass them via the `binspos` argument as a numeric vector.
#'
#' @references
#' Cattaneo, M. D., R. K. Crump, M. H. Farrell, and Y. Feng (2024).
#' On Binscatter. \emph{American Economic Review}, 114(5): 1488-1514.
#'
#' @export
#' @examples
#' \dontrun{
#' ChickWeight = as.data.frame(ChickWeight)
#' 
#' # Canonical binscatter: bin means (default)
#' dbbin(weight ~ Time, ChickWeight, nbins = 10)
#'
#' # Piecewise linear, no smoothness
#' dbbin(weight ~ Time, ChickWeight, nbins = 10, dots = c(1, 0))
#'
#' # Piecewise quadratic with C1 continuity
#' dbbin(weight ~ Time, ChickWeight, nbins = 10, dots = c(2, 1))
#'
#' # With line overlay for smooth visualization
#' dbbin(weight ~ Time, ChickWeight, nbins = 10, dots = c(0, 0), line = c(1, 1))
#'
#' # With fixed effects (diet type)
#' dbbin(weight ~ Time | Diet, ChickWeight, nbins = 10, dots = c(1, 0))
#' }
dbbin = function(
  fml,
  data,
  dots = c(0, 0),
  line = NULL,
  linegrid = 20,
  nbins = 20,
  binspos = "qs",
  sample_frac = NULL,
  ci = TRUE,
  vcov = NULL,
  level = 95,
  strategy = "auto",
  conn = NULL,
  verbose = TRUE
) {
  
  # -------------------------------------------------------------------------
  # Process binsreg-style arguments: dots, line, binspos
  # -------------------------------------------------------------------------
  
  # Parse dots argument: c(p, s) or TRUE/FALSE/NULL
  dots_p = 0L
  dots_s = 0L
  dots_on = TRUE
  
  if (is.null(dots) || identical(dots, FALSE)) {
    dots_on = FALSE
  } else if (identical(dots, TRUE)) {
    dots_p = 0L
    dots_s = 0L
  } else if (is.numeric(dots) && length(dots) == 2) {
    dots_p = as.integer(dots[1])
    dots_s = as.integer(dots[2])
    if (dots_s > dots_p) {
      stop("dots: smoothness s must be <= degree p (got dots = c(", dots_p, ", ", dots_s, "))")
    }
    # Note: p > 2 is now supported via truncated power spline basis
  } else {
    stop("dots must be NULL, FALSE, TRUE, or a vector c(p, s) with p = degree, s = smoothness")
  }
  
  # Parse line argument: c(p, s) or TRUE/FALSE/NULL
  line_p = NULL
  line_s = NULL
  line_on = FALSE
  
  if (!is.null(line) && !identical(line, FALSE)) {
    line_on = TRUE
    if (identical(line, TRUE)) {
      # Default to same as dots if TRUE
      line_p = dots_p
      line_s = dots_s
    } else if (is.numeric(line) && length(line) == 2) {
      line_p = as.integer(line[1])
      line_s = as.integer(line[2])
      if (line_s > line_p) {
        stop("line: smoothness s must be <= degree p (got line = c(", line_p, ", ", line_s, "))")
      }
      # Note: p > 2 is now supported via truncated power spline basis
    } else {
      stop("line must be NULL, FALSE, TRUE, or a vector c(p, s)")
    }
  }
  
  # Parse binspos argument: "qs", "es", or numeric vector
  if (is.character(binspos)) {
    if (!binspos %in% c("qs", "es")) {
      stop("binspos must be 'qs' (quantile-spaced), 'es' (evenly-spaced), or a numeric vector of knots")
    }
    partition_method = if (binspos == "qs") "quantile" else "equal"
    breaks = NULL
  } else if (is.numeric(binspos)) {
    # Manual knot positions
    partition_method = "manual"
    breaks = sort(binspos)
    nbins = length(breaks) - 1
    if (nbins < 1) {
      stop("binspos must have at least 2 values when specifying manual knots")
    }
  } else {
    stop("binspos must be 'qs', 'es', or a numeric vector of knot positions")
  }
  
  # Convert binsreg parameters to internal parameters
  # For now, we use the dots parameters as the primary model
  degree = dots_p

  smooth = dots_s
  B = as.integer(nbins)
  
  # Validate linegrid
  if (!is.numeric(linegrid) || linegrid < 1) {
    stop("linegrid must be a positive integer")
  }
  linegrid = as.integer(linegrid)
  
  # Validate sample_frac (NULL is allowed for auto behavior)
  if (!is.null(sample_frac)) {
    if (!is.numeric(sample_frac) || length(sample_frac) != 1 || sample_frac <= 0 || sample_frac > 1) {
      stop("sample_frac must be NULL or a numeric value between 0 and 1")
    }
  }
  
  # Validate level (now as percentage, e.g., 95 for 95% CI)
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 100) {
    stop("level must be a numeric value between 0 and 100 (e.g., 95 for 95% CI)")
  }
  # Convert to alpha for internal use
  alpha = 1 - level / 100
  
  # Handle vcov / ci interaction
  # Default to HC1 (heteroskedasticity-robust) to match binsreg package
  if (isTRUE(ci) && is.null(vcov)) {
    vcov = "HC1"
  }
  
  # Extract cluster variable if vcov is a formula
  cluster_var = NULL
  if (inherits(vcov, "formula")) {
    cluster_var = all.vars(vcov)
    if (length(cluster_var) != 1) {
      stop("vcov formula must specify exactly one clustering variable (e.g., ~cluster_id)")
    }
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
  
  # Validate derived parameters
  if (!is.numeric(B) || B < 1) {
    stop("nbins must be a positive integer")
  }
  
  # Warn if user sets strategy when `s` (smoothness) > 0 (it will be ignored)
  if (smooth > 0 && strategy != "auto") {
    warning("'strategy' parameter is ignored when `s` (smoothness) > 0 (constrained estimation)", 
            call. = FALSE)
  }
  
  # Set up database connection if needed
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
      # Create temp table from subquery using raw SQL
      table_name = sprintf("__db_bins_%s_input", 
                          gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
      subquery_sql = dbplyr::sql_render(data)
      DBI::dbExecute(conn, glue("CREATE TEMPORARY TABLE {table_name} AS {subquery_sql}"))
      temp_tables = c(temp_tables, table_name)
    }
  } else if (is.data.frame(data)) {
    # Copy R dataframe to temp table
    table_name = sprintf("__db_bins_%s_input", 
                        gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
    DBI::dbWriteTable(conn, table_name, data, temporary = TRUE)
    temp_tables = c(temp_tables, table_name)
  } else {
    stop("data must be a dataframe, table name, or lazy table (e.g., from dplyr::tbl())")
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
  
  # -------------------------------------------------------------------------
  # SAMPLING: Compute breaks from sample if dataset is large
  # -------------------------------------------------------------------------
  # Auto behavior (sample_frac = NULL): 
  #   - If >1M rows: sample 10% for bin boundary computation
  #   - Otherwise: use all data
  # If user provides explicit sample_frac, use that regardless of row count.
  # This avoids expensive NTILE() or min/max scans on very large datasets.
  
  # Get backend for SQL dialect
  backend_info = detect_backend(conn)
  backend = backend_info$name
  
  sampled = FALSE
  if (partition_method != "manual") {
    # Get row count using direct SQL
    count_expr = dbbin_sql_count(backend)
    count_sql = glue("
      SELECT {count_expr} AS n 
      FROM {table_name} 
      WHERE {x_name} IS NOT NULL AND {y_name} IS NOT NULL
    ")
    n_rows = DBI::dbGetQuery(conn, count_sql)$n
    
    # Determine effective sample_frac
    if (is.null(sample_frac)) {
      # Auto: 10% if >1M rows, 100% otherwise
      effective_sample_frac = if (n_rows > 1e6) 0.1 else 1
    } else {
      # User override
      effective_sample_frac = sample_frac
    }
    
    # Sample if fraction < 1
    if (effective_sample_frac < 1) {
      if (verbose) {
        message(sprintf("Dataset has %.1fM rows, computing breaks from %.0f%% sample...", 
                       n_rows / 1e6, effective_sample_frac * 100))
      }
      
      # Sample data for break computation using direct SQL
      sample_size = max(10000L, ceiling(n_rows * effective_sample_frac))  # at least 10k rows
      random_expr = dbbin_sql_random(backend)
      
      sample_sql = glue("
        SELECT {x_name}
        FROM {table_name}
        WHERE {x_name} IS NOT NULL AND {y_name} IS NOT NULL
        ORDER BY {random_expr}
        LIMIT {sample_size}
      ")
      sampled_data = DBI::dbGetQuery(conn, sample_sql)
      x_sample = sampled_data[[x_name]]
      
      # Compute breaks based on partition method
      if (partition_method == "quantile") {
        # Quantile breaks for equal-count bins
        probs = seq(0, 1, length.out = B + 1)
        breaks = quantile(x_sample, probs = probs, na.rm = TRUE, names = FALSE)
      } else if (partition_method == "equal") {
        # Equal-width breaks
        x_min = min(x_sample, na.rm = TRUE)
        x_max = max(x_sample, na.rm = TRUE)
        breaks = seq(x_min, x_max, length.out = B + 1)
      } else if (partition_method == "log_equal") {
        # Equal-width breaks in log space
        x_pos = x_sample[x_sample > 0]
        if (length(x_pos) == 0) {
          stop("log_equal partition requires positive x values")
        }
        log_min = log(min(x_pos, na.rm = TRUE))
        log_max = log(max(x_pos, na.rm = TRUE))
        breaks = exp(seq(log_min, log_max, length.out = B + 1))
      }
      
      # Switch to manual partitioning with computed breaks
      partition_method = "manual"
      sampled = TRUE
      
      if (verbose) {
        message(sprintf("Computed %d breaks from sample, using manual partitioning.", length(breaks)))
      }
    }
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
    cluster_var = cluster_var,
    partition_method = partition_method,
    breaks = breaks,
    ci = ci,
    vcov = vcov,
    level = alpha,  # Internal: use alpha (0.05), not level (95)
    strategy = strategy,
    verbose = verbose,
    formula = as.formula(formula_str),
    # binsreg-style parameters
    dots = if (dots_on) c(dots_p, dots_s) else NULL,
    line = if (line_on) c(line_p, line_s) else NULL,
    linegrid = linegrid,
    dots_on = dots_on,
    line_on = line_on,
    binspos = binspos
  )
  
  # -------------------------------------------------------------------------
  # DISPATCH: Execute estimation based on dots/line parameters
  # -------------------------------------------------------------------------
  # Following binsreg logic:
  # - If dots and line have same (p,s): fit one model, use for both
  # - If different: fit separate models
  # - smooth > 0 uses constrained splines, smooth == 0 uses unconstrained piecewise
  
  # Determine if we need separate models
  need_separate_models = line_on && dots_on && 
    (dots_p != line_p || dots_s != line_s)
  
  if (need_separate_models) {
    # Fit separate models for dots and line
    result = execute_separate_binsreg(inputs)
  } else {
    # Single model for both dots and line
    # Use unconstrained path for s=0 (bin dummies, can use compress strategy)
    # Use constrained path for s>0 (spline basis, requires row-level data)
    if (smooth == 0) {
      result = execute_unconstrained_binsreg(inputs)
    } else {
      result = execute_constrained_binsreg(inputs)
    }
  }
  
  return(result)
}

# =============================================================================
# Internal SQL helpers for backend-agnostic operations
# =============================================================================

#' Get backend-specific random ordering expression
#' @param backend Backend name from detect_backend()
#' @return SQL expression for random ordering
#' @keywords internal
dbbin_sql_random <- function(backend) {
  switch(backend,
    "duckdb" = "RANDOM()",
    "sqlserver" = "NEWID()",
    "postgres" = "RANDOM()",
    "sqlite" = "RANDOM()",
    "mysql" = "RAND()",
    "RANDOM()"  # default fallback
  )
}

#' Get backend-specific count expression
#' @param backend Backend name from detect_backend()
#' @return SQL expression for counting rows
#' @keywords internal
dbbin_sql_count <- function(backend) {
  if (backend == "sqlserver") "COUNT_BIG(*)" else "COUNT(*)"
}

#' Get NTILE window function expression
#' @param x_name Column name to partition by
#' @param n_bins Number of bins
#' @return SQL NTILE expression
#' @keywords internal
dbbin_sql_ntile <- function(x_name, n_bins) {
  glue("NTILE({n_bins}) OVER (ORDER BY {x_name})")
}


#' Execute separate binsreg models for dots and line
#' 
#' When dots and line have different (p,s) parameters, we need to fit two separate
#' models following binsreg's approach. This function:
#' 1. Fits a model with dots parameters (dots.p, dots.s) for data.dots
#' 2. Fits a model with line parameters (line.p, line.s) for data.line
#' 3. Combines results into single dbbin output
#' 
#' @keywords internal
execute_separate_binsreg = function(inputs) {
  
  if (inputs$verbose) {
    cat("[dbbin] Fitting separate models for dots and line (different parameters)\n")
    cat(sprintf("        dots = c(%d, %d), line = c(%d, %d)\n",
                inputs$dots[1], inputs$dots[2], inputs$line[1], inputs$line[2]))
  }
  
  # Save original parameters
  original_dots = inputs$dots
  original_line = inputs$line
  
  # -------------------------------------------------------------------------
  # Step 1: Fit model for DOTS using dots parameters
  # -------------------------------------------------------------------------
  dots_inputs = inputs
  dots_inputs$degree = original_dots[1]
  dots_inputs$smooth = original_dots[2]
  dots_inputs$line_on = FALSE  # Only compute dots
  dots_inputs$dots_on = TRUE
  
  if (dots_inputs$smooth == 0) {
    dots_result = execute_unconstrained_binsreg(dots_inputs)
  } else {
    dots_result = execute_constrained_binsreg(dots_inputs)
  }
  
  # -------------------------------------------------------------------------
  # Step 2: Fit model for LINE using line parameters
  # -------------------------------------------------------------------------
  line_inputs = inputs
  line_inputs$degree = original_line[1]
  line_inputs$smooth = original_line[2]
  line_inputs$line_on = TRUE  # Only compute line
  line_inputs$dots_on = FALSE
  line_inputs$ci = FALSE  # CIs only for dots
  
  if (line_inputs$smooth == 0) {
    line_result = execute_unconstrained_binsreg(line_inputs)
  } else {
    line_result = execute_constrained_binsreg(line_inputs)
  }
  
  # -------------------------------------------------------------------------
  # Step 3: Combine results
  # -------------------------------------------------------------------------
  result = list(
    data.dots = dots_result$data.dots,
    data.line = line_result$data.line,
    data.bin = dots_result$data.bin,  # Same for both
    model.dots = dots_result$model,
    model.line = line_result$model,
    opt = list(
      dots = original_dots,
      line = original_line,
      nbins = dots_result$opt$nbins,
      binspos = inputs$binspos,
      N = dots_result$opt$N,
      x_var = inputs$x_name,
      y_var = inputs$y_name,
      formula = inputs$formula,
      level = dots_result$opt$level,
      ci = inputs$ci,
      vcov = inputs$vcov
    )
  )
  
  # Add knots if available
  if (!is.null(dots_result$knots)) {
    result$knots.dots = dots_result$knots
  }
  if (!is.null(line_result$knots)) {
    result$knots.line = line_result$knots
  }
  
  class(result) = "dbbin"
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
  controls = inputs$controls
  fe = inputs$fe
  cluster_var = inputs$cluster_var
  
  # Build column list
  cols = c(y_name, x_name)
  if (!is.null(controls)) {
    cols = c(cols, controls)
  }
  if (!is.null(fe)) {
    cols = c(cols, fe)
  }
  if (!is.null(cluster_var)) {
    cols = c(cols, cluster_var)
  }
  
  # Weight expression (no weights support for now)
  wt_expr = "1.0"
  
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
  # Note: NTILE() differs slightly from binsreg's quantile(type=2). See ?dbbin details.
  
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
      c(x_left = min(x), x_right = max(x), x_mid = (min(x) + max(x)) / 2, 
        x_mean = mean(x), n = length(x))
    }
  )
  
  # Unpack the matrix column
  geo_mat = geo$x
  geo = data.frame(
    bin = geo$bin,
    x_left = geo_mat[, "x_left"],
    x_right = geo_mat[, "x_right"],
    x_mid = geo_mat[, "x_mid"],
    x_mean = geo_mat[, "x_mean"],
    n = geo_mat[, "n"]
  )
  
  return(geo)
}


#' Add normalized polynomial basis to binned data (binsreg compatible)
#' 
#' Uses binsreg's basis construction: u = (x - x_left) / h where h is bin width.
#' This normalizes u to [0, 1] within each bin, matching binsreg's polynomial 
#' parameterization for s=0 (unconstrained) estimation.
#' 
#' @keywords internal
add_basis_columns = function(binned_data, geo, x_name, degree) {
  
  if (degree == 0) {
    return(binned_data)  # No basis needed for means
  }
  

  # Merge in bin geometry (x_left and bin width h)
  geo$h = geo$x_right - geo$x_left
  binned_data = merge(binned_data, geo[, c("bin", "x_left", "h")], by = "bin")
  
  # Add normalized polynomial terms matching binsreg: u = (x - x_left) / h
  # This gives u in [0, 1] for each bin
  binned_data$u = (binned_data[[x_name]] - binned_data$x_left) / binned_data$h
  
  # Add higher-order terms only if degree >= 2
  # Note: In R, 2:1 produces c(2,1) not empty, so we need explicit check
  if (degree >= 2) {
    for (d in 2:degree) {
      col_name = paste0("u", d)
      binned_data[[col_name]] = binned_data$u^d
    }
  }
  
  return(binned_data)
}

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
      
      # Create higher-order polynomial terms: u2_i, u3_i, ..., u{degree}_i
      # Note: In R, 2:1 produces c(2,1) not empty, so we need explicit check
      if (inputs$degree >= 2) {
        for (d in 2:inputs$degree) {
          col_name_d = paste0("u", d, "_", bin_num)
          u_col = paste0("u", d)
          binned_data[[col_name_d]] = ifelse(binned_data$bin == bin_val, binned_data[[u_col]], 0)
        }
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
    
  } else {
    # Piecewise polynomial: y ~ 0 + bin + u_1 + ... [+ u2_1 + ... + u{degree}_1 + ...]
    present_bins = sort(as.integer(as.character(unique(binned_data$bin))))
    
    # Linear terms
    u_terms = paste0("u_", present_bins, collapse = " + ")
    fml_rhs = paste("0 + bin", u_terms, sep = " + ")
    
    # Higher-order polynomial terms
    # Note: In R, 2:1 produces c(2,1) not empty, so we need explicit check
    if (degree >= 2) {
      for (d in 2:degree) {
        ud_terms = paste0("u", d, "_", present_bins, collapse = " + ")
        fml_rhs = paste(fml_rhs, ud_terms, sep = " + ")
      }
    }
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
#' Uses regression splines with truncated power basis to enforce continuity 
#' constraints at bin boundaries.
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
  controls = inputs$controls
  fe = inputs$fe
  vcov_type = if (isTRUE(inputs$ci)) inputs$vcov else "iid"
  
  # Get backend info
  backend_info = detect_backend(conn)
  backend = backend_info$name
  count_expr = dbbin_sql_count(backend)
  
  # Create temp table name for binned data
  binned_table = sprintf("__dbbin_%s_binned", 
                         gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
  
  # Step 1: Create binned data table with bin assignments
  if (partition_method == "quantile") {
    # NTILE-based quantile binning
    ntile_expr = dbbin_sql_ntile(x_name, B)
    bin_sql = glue("
      CREATE TEMPORARY TABLE {binned_table} AS
      SELECT *, {ntile_expr} AS bin
      FROM {table_name}
      WHERE {x_name} IS NOT NULL AND {y_name} IS NOT NULL
    ")
    DBI::dbExecute(conn, bin_sql)
    
  } else if (partition_method == "equal") {
    # Equal-width binning using window functions for min/max
    bin_sql = glue("
      CREATE TEMPORARY TABLE {binned_table} AS
      SELECT t2.*,
             CASE WHEN raw_bin > {B} THEN {B} ELSE CAST(raw_bin AS INTEGER) END AS bin
      FROM (
        SELECT *,
               1 + FLOOR(({x_name} - min_x) / NULLIF((max_x - min_x) / {B}, 0)) AS raw_bin
        FROM (
          SELECT *,
                 MIN({x_name}) OVER () AS min_x,
                 MAX({x_name}) OVER () AS max_x
          FROM {table_name}
          WHERE {x_name} IS NOT NULL AND {y_name} IS NOT NULL
        ) t1
      ) t2
    ")
    DBI::dbExecute(conn, bin_sql)
    
  } else if (partition_method == "manual") {
    # Manual breaks: assign bin based on user-specified breakpoints
    breaks = inputs$breaks
    n_bins = length(breaks) - 1
    
    # Build CASE WHEN expression for bin assignment
    case_parts = character()
    for (i in 1:(n_bins - 1)) {
      case_parts = c(case_parts, glue("WHEN {x_name} < {breaks[i + 1]} THEN {i}"))
    }
    case_parts = c(case_parts, glue("ELSE {n_bins}"))
    case_expr = paste("CASE", paste(case_parts, collapse = " "), "END")
    
    bin_sql = glue("
      CREATE TEMPORARY TABLE {binned_table} AS
      SELECT *, {case_expr} AS bin
      FROM {table_name}
      WHERE {x_name} IS NOT NULL AND {y_name} IS NOT NULL
        AND {x_name} >= {breaks[1]} AND {x_name} <= {breaks[length(breaks)]}
    ")
    DBI::dbExecute(conn, bin_sql)
    
    # Update B to match number of bins from breaks
    B = n_bins
  } else {
    stop("partition_method = '", partition_method, "' not supported")
  }
  
  # Ensure cleanup of temp table
  on.exit(try(DBI::dbRemoveTable(conn, binned_table, fail_if_missing = FALSE), silent = TRUE), add = TRUE)
  
  # Compute bin geometry (boundaries and counts) using direct SQL
  geo_sql = glue("
    SELECT bin,
           MIN({x_name}) AS x_left,
           MAX({x_name}) AS x_right,
           (MIN({x_name}) + MAX({x_name})) / 2.0 AS x_mid,
           AVG({x_name}) AS x_mean,
           {count_expr} AS n
    FROM {binned_table}
    GROUP BY bin
    ORDER BY bin
  ")
  geo = DBI::dbGetQuery(conn, geo_sql)
  
  # Step 2: Extract interior knots from bin boundaries
  # Knots are at x_right[1], x_right[2], ..., x_right[B-1]
  knots = geo$x_right[1:(B-1)]
  
  if (inputs$verbose) {
    cat("[dbbin] Using ", length(knots), " interior knots for spline basis\n", sep = "")
  }
  
  # Step 3: Build spline basis columns using SQL
  # We need to add columns to the binned table for the spline basis
  basis_names = character()
  basis_exprs = character()
  
  # Global polynomial part (excluding intercept, which dbreg handles)
  # x, x^2, x^3, ..., x^degree
  for (d in seq_len(degree)) {
    col_name = paste0("x", d, "_spline")
    basis_names = c(basis_names, col_name)
    if (d == 1) {
      # x^1 = x (no POWER needed)
      basis_exprs = c(basis_exprs, glue("{x_name} AS {col_name}"))
    } else {
      # x^d using POWER()
      basis_exprs = c(basis_exprs, glue("POWER({x_name}, {d}) AS {col_name}"))
    }
  }
  
  # Truncated power terms at each knot: (x - κ_j)₊^r for r = smooth, ..., degree
  # When degree=0 and smooth=0, r=0 creates step functions (bin indicators)
  for (j in seq_along(knots)) {
    kappa = knots[j]
    for (r in smooth:degree) {
      col_name = sprintf("knot%d_pow%d", j, r)
      
      if (r == 0) {
        # (x - κ)₊⁰ = step function: 1 if x > κ, 0 otherwise (bin indicator shift)
        expr = glue("CASE WHEN {x_name} > {kappa} THEN 1.0 ELSE 0.0 END AS {col_name}")
      } else if (r == 1) {
        # (x - κ)₊ = CASE WHEN x > κ THEN x - κ ELSE 0 END
        expr = glue("CASE WHEN {x_name} > {kappa} THEN {x_name} - {kappa} ELSE 0 END AS {col_name}")
      } else if (r == 2) {
        # (x - κ)₊² = CASE WHEN x > κ THEN (x - κ)² ELSE 0 END
        expr = glue("CASE WHEN {x_name} > {kappa} THEN ({x_name} - {kappa}) * ({x_name} - {kappa}) ELSE 0 END AS {col_name}")
      } else {
        # General case for r > 2
        expr = glue("CASE WHEN {x_name} > {kappa} THEN POWER({x_name} - {kappa}, {r}) ELSE 0 END AS {col_name}")
      }
      basis_names = c(basis_names, col_name)
      basis_exprs = c(basis_exprs, expr)
    }
  }
  
  # Create new table with basis columns (replace binned table)
  spline_table = sprintf("__dbbin_%s_spline", 
                         gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
  
  basis_cols_sql = paste(basis_exprs, collapse = ",\n           ")
  spline_sql = glue("
    CREATE TEMPORARY TABLE {spline_table} AS
    SELECT *,
           {basis_cols_sql}
    FROM {binned_table}
  ")
  DBI::dbExecute(conn, spline_sql)
  on.exit(try(DBI::dbRemoveTable(conn, spline_table, fail_if_missing = FALSE), silent = TRUE), add = TRUE)
  
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
  # Pass the table name as `table` argument
  fit = dbreg(
    fml = fml,
    table = spline_table,  # Pass table name
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
    
    # Global polynomial part: x^1, x^2, ..., x^degree
    for (d in seq_len(degree)) {
      col_name = paste0("x", d, "_spline")
      if (col_name %in% basis_names) {
        bvec[col_name] = x_val^d
      }
    }
    
    # Knot features
    for (j in seq_along(knots)) {
      for (r in smooth:degree) {
        col_name = sprintf("knot%d_pow%d", j, r)
        if (col_name %in% basis_names) {
          if (r == 0) {
            # Step function: 1 if x > knot, 0 otherwise
            bvec[col_name] = as.numeric(x_val > knots[j])
          } else {
            # Truncated power: (x - κ)₊^r
            bvec[col_name] = pmax(0, x_val - knots[j])^r
          }
        }
      }
    }
    bvec
  }
  
  # Evaluation function: ŷ(x) = intercept + b(x)' β
  # Note: bin argument is ignored for splines (global fit)
  eval_fn = function(x_val, bin) {
    bvec = build_basis_vector(x_val)
    beta_basis = coefs[basis_names]
    beta_basis[is.na(beta_basis)] = 0
    intercept + sum(bvec * beta_basis)
  }
  
  # SE function: sqrt(b(x)' V b(x))
  se_fn = if (!is.null(V)) {
    function(x_val, bin) {
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
  } else NULL
  
  # Use the unified output builder
  build_dbbin_output(inputs, fit, geo, eval_fn, se_fn, knots = knots)
}


#' Construct output from unconstrained binned regression
#' 
#' @description Internal helper that transforms coefficient estimates from unconstrained
#' binned regression into the standard dbbin output list format.
#' 
#' @keywords internal
construct_output = function(inputs, fit, geo, V_beta = NULL) {
  
  degree = inputs$degree
  B = nrow(geo)
  
  # Extract coefficients from coeftable
  if (is.null(fit$coeftable)) {
    stop("No coefficients found in fit object")
  }
  
  coef_names = rownames(fit$coeftable)
  coef_vals = fit$coeftable[, "estimate"]
  names(coef_vals) = coef_names
  
  has_intercept = "(Intercept)" %in% coef_names
  
  # Helper to get SE of linear combination: sqrt(w' V w)
  get_se = function(coef_indices, weights) {
    if (is.null(V_beta)) return(NA_real_)
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
  
  # Build evaluation function based on degree
  if (degree == 0) {
    # Degree 0: bin means (constant within bin)
    eval_fn = function(x_val, bin) {
      bin_col = paste0("bin", bin)
      if (!has_intercept) {
        if (bin_col %in% coef_names) coef_vals[bin_col] else NA_real_
      } else {
        if (bin == 1) {
          coef_vals["(Intercept)"]
        } else {
          coef_vals["(Intercept)"] + (if (bin_col %in% coef_names) coef_vals[bin_col] else 0)
        }
      }
    }
    
    se_fn = if (!is.null(V_beta)) {
      function(x_val, bin) {
        bin_col = paste0("bin", bin)
        if (!has_intercept) {
          if (bin_col %in% coef_names) get_se(bin_col, 1) else NA_real_
        } else {
          if (bin == 1) {
            get_se("(Intercept)", 1)
          } else {
            if (bin_col %in% coef_names) {
              get_se(c("(Intercept)", bin_col), c(1, 1))
            } else {
              get_se("(Intercept)", 1)
            }
          }
        }
      }
    } else NULL
    
  } else {
    # Degree >= 1: piecewise polynomial using normalized basis (binsreg compatible)
    # u = (x - x_left) / h where h is bin width, giving u in [0, 1]
    eval_fn = function(x_val, bin) {
      bin_col = paste0("bin", bin)
      bin_idx = which(geo$bin == bin)
      if (length(bin_idx) == 0) return(NA_real_)
      
      # Normalized basis: u = (x - x_left) / h
      x_left = geo$x_left[bin_idx]
      h = geo$x_right[bin_idx] - x_left
      u = (x_val - x_left) / h
      
      # Get intercept (b0)
      if (!has_intercept) {
        b0 = if (bin_col %in% coef_names) coef_vals[bin_col] else 0
      } else {
        if (bin == 1) {
          b0 = coef_vals["(Intercept)"]
        } else {
          b0 = coef_vals["(Intercept)"] + (if (bin_col %in% coef_names) coef_vals[bin_col] else 0)
        }
      }
      
      # Linear term
      b1_name = sprintf("u_%d", bin)
      b1 = if (b1_name %in% coef_names) coef_vals[b1_name] else 0
      
      # Start with intercept + linear term
      result = b0 + b1 * u
      
      # Higher-order polynomial terms: u^2, u^3, ..., u^degree
      # Note: In R, 2:1 produces c(2,1) not empty, so we need explicit check
      if (degree >= 2) {
        for (d in 2:degree) {
          bd_name = sprintf("u%d_%d", d, bin)
          bd = if (bd_name %in% coef_names) coef_vals[bd_name] else 0
          result = result + bd * u^d
        }
      }
      
      result
    }
    
    se_fn = if (!is.null(V_beta)) {
      function(x_val, bin) {
        bin_col = paste0("bin", bin)
        bin_idx = which(geo$bin == bin)
        if (length(bin_idx) == 0) return(NA_real_)
        
        # Normalized basis: u = (x - x_left) / h
        x_left = geo$x_left[bin_idx]
        h = geo$x_right[bin_idx] - x_left
        u = (x_val - x_left) / h
        
        # Build coefficient indices and weights
        if (!has_intercept) {
          b0_idx = if (bin_col %in% coef_names) bin_col else NULL
          b0_w = if (!is.null(b0_idx)) 1 else NULL
        } else {
          if (bin == 1) {
            b0_idx = "(Intercept)"
            b0_w = 1
          } else {
            b0_idx = c("(Intercept)", if (bin_col %in% coef_names) bin_col else NULL)
            b0_w = c(1, if (bin_col %in% coef_names) 1 else NULL)
          }
        }
        
        # Linear term
        b1_name = sprintf("u_%d", bin)
        b1_idx = if (b1_name %in% coef_names) b1_name else NULL
        
        idx = c(b0_idx, b1_idx)
        w = c(b0_w, if (!is.null(b1_idx)) u else NULL)
        
        # Higher-order polynomial terms: u^2, u^3, ..., u^degree
        # Note: In R, 2:1 produces c(2,1) not empty, so we need explicit check
        if (degree >= 2) {
          for (d in 2:degree) {
            bd_name = sprintf("u%d_%d", d, bin)
            bd_idx = if (bd_name %in% coef_names) bd_name else NULL
            if (!is.null(bd_idx)) {
              idx = c(idx, bd_idx)
              w = c(w, u^d)
            }
          }
        }
        
        if (length(idx) == 0) return(NA_real_)
        get_se(idx, w)
      }
    } else NULL
  }
  
  # Use the new unified output builder
  build_dbbin_output(inputs, fit, geo, eval_fn, se_fn)
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


#' Build dbbin output list (binsreg-compatible format)
#' 
#' @description Creates the standard dbbin output structure matching binsreg's
#' output format: data.dots (evaluated at bin means), data.line (on grid),
#' and data.bin (bin geometry).
#' 
#' @param inputs List of input parameters from dbbin()
#' @param fit The dbreg model object
#' @param geo Data frame with bin geometry (x_left, x_right, x_mid, n, x_mean)
#' @param eval_fn Function that takes (x, bin) and returns fitted y values
#' @param se_fn Function that takes (x, bin) and returns standard errors (or NULL)
#' @param knots Optional vector of knots (for constrained estimation)
#' 
#' @return A list with class "dbbin" containing:
#'   - data.dots: data.frame with columns x, bin, fit, se, ci.l, ci.r (at bin means)
#'   - data.line: data.frame with columns x, bin, fit (on grid) - only if line requested
#'   - data.bin: data.frame with bin.id, left.endpoint, right.endpoint
#'   - model: the dbreg fit object
#'   - opt: list of options (dots, line, nbins, binspos, etc.)
#' 
#' @keywords internal
build_dbbin_output = function(inputs, fit, geo, eval_fn, se_fn = NULL, knots = NULL) {
  
  B = nrow(geo)
  level = inputs$level  # This is alpha (e.g., 0.05)
  crit_val = stats::qnorm(1 - level / 2)
  linegrid = inputs$linegrid
  
  # -------------------------------------------------------------------------
  # Build data.dots: evaluate at bin means (binsreg style)
  # -------------------------------------------------------------------------
  if (isTRUE(inputs$dots_on)) {
    # Evaluate at bin means
    x_mean = geo$x_mean
    fit_dots = sapply(seq_len(B), function(i) eval_fn(x_mean[i], geo$bin[i]))
    
    if (!is.null(se_fn) && isTRUE(inputs$ci)) {
      se_dots = sapply(seq_len(B), function(i) se_fn(x_mean[i], geo$bin[i]))
      ci_l = fit_dots - crit_val * se_dots
      ci_r = fit_dots + crit_val * se_dots
    } else {
      se_dots = rep(NA_real_, B)
      ci_l = rep(NA_real_, B)
      ci_r = rep(NA_real_, B)
    }
    
    data_dots = data.frame(
      x = x_mean,
      bin = geo$bin,
      fit = fit_dots,
      se = se_dots,
      ci.l = ci_l,
      ci.r = ci_r
    )
    rownames(data_dots) = NULL
  } else {
    data_dots = NULL
  }
  
  # -------------------------------------------------------------------------
  # Build data.line: evaluate on grid within each bin
  # -------------------------------------------------------------------------
  if (isTRUE(inputs$line_on)) {
    line_list = vector("list", B)
    
    for (i in seq_len(B)) {
      # Generate grid points within bin
      x_seq = seq(geo$x_left[i], geo$x_right[i], length.out = linegrid)
      fit_line = sapply(x_seq, eval_fn, bin = geo$bin[i])
      
      line_list[[i]] = data.frame(
        x = x_seq,
        bin = geo$bin[i],
        fit = fit_line
      )
    }
    
    data_line = do.call(rbind, line_list)
    rownames(data_line) = NULL
  } else {
    data_line = NULL
  }
  
  # -------------------------------------------------------------------------
  # Build data.bin: bin geometry (binsreg format)
  # -------------------------------------------------------------------------
  data_bin = data.frame(
    bin.id = geo$bin,
    left.endpoint = geo$x_left,
    right.endpoint = geo$x_right
  )
  rownames(data_bin) = NULL
  
  # -------------------------------------------------------------------------
  # Build opt: options list (binsreg style)
  # -------------------------------------------------------------------------
  opt = list(
    dots = inputs$dots,
    line = inputs$line,
    nbins = B,
    binspos = inputs$binspos,
    N = sum(geo$n),
    x_var = inputs$x_name,
    y_var = inputs$y_name,
    formula = inputs$formula,
    level = (1 - level) * 100,  # Convert back to percentage for display
    ci = inputs$ci,
    vcov = inputs$vcov
  )
  
  # -------------------------------------------------------------------------
  # Build result list
  # -------------------------------------------------------------------------
  result = list(
    data.dots = data_dots,
    data.line = data_line,
    data.bin = data_bin,
    model = fit,
    opt = opt
  )
  
  # Add knots if provided (constrained estimation)
  if (!is.null(knots)) {
    result$knots = knots
  }
  
  class(result) = "dbbin"
  return(result)
}