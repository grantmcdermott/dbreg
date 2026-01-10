# =============================================================================
# dbevent: Event Study Regression for Database-Backed Data
# =============================================================================

#' Event Study Regression
#'
#' Estimate event study (difference-in-differences with dynamic effects) using
#' database-backed data. Automatically generates treatment×time indicator
#' variables and estimates effects relative to a reference period.
#'
#' @param outcome Character: name of the outcome variable column.
#' @param data A data.frame or dbplyr lazy table.
#' @param conn A DBI database connection.
#' @param table Name of the table in the database (character string).
#' @param path Path to a parquet, csv, or other file readable by DuckDB.
#' @param unit Character: name of the unit (panel) identifier column.
#' @param time Character: name of the time period column.
#' @param treat Character: name of the binary treatment indicator column (0/1).
#' @param covariates Character vector: names of additional control variables
#'   to include in the regression (optional).
#' @param ref Integer or vector of integers: reference period(s) relative to
#'   treatment (default -1, i.e., one period before treatment).
#' @param leads Integer: number of pre-treatment periods to include. If NULL,
#'   auto-detected from the data.
#' @param lags Integer: number of post-treatment periods to include. If NULL,
#'   auto-detected from the data.
#' @param vcov Character or formula: type of variance-covariance estimator.
#'   Default is "cluster" which clusters on the unit variable.
#' @param cluster Formula for clustering (e.g., `~firm`). Defaults to clustering
#'   on the unit identifier if vcov = "cluster".
#' @param verbose Logical: print progress messages.
#' @param ... Additional arguments passed to [dbreg()].
#'
#' @return An object of class "dbevent" containing:
#'   - `model`: The underlying dbreg model object
#'   - `coefs`: Data frame of event study coefficients with standard errors and CIs
#'   - `event_time`: List with leads, lags, and reference periods
#'   - `validation`: Results from data validation checks
#'   - `call`: The matched call
#'   - `spec`: List with unit, time, treat column names
#'
#' @details
#' This function implements a two-way fixed effects event study estimator.
#' It automatically:
#' 1. Validates the panel structure (unique unit-time, absorbing treatment, etc.)
#' 2. Computes cohorts (first treatment period for each unit)
#' 3. Creates event-time indicator variables
#' 4. Estimates effects using [dbreg()] with unit and time fixed effects
#'
#' The treatment variable must be binary (0/1) and absorbing (once treated,
#' always treated). Units can be never-treated (always 0) or eventually treated.
#'
#' @examples
#' \dontrun{
#' # Simulate panel data
#' library(duckdb)
#' con <- dbConnect(duckdb())
#'
#' # Basic event study
#' es <- dbevent(
#'   outcome = "y",
#'   conn = con,
#'   table = "panel_data",
#'   unit = "firm_id",
#'   time = "year",
#'   treat = "treated",
#'   ref = -1,
#'   leads = 5,
#'   lags = 5
#' )
#'
#' # With covariates
#' es <- dbevent(
#'   outcome = "y",
#'   conn = con,
#'   table = "panel_data",
#'   unit = "firm_id",
#'   time = "year",
#'   treat = "treated",
#'   covariates = c("x1", "x2")
#' )
#'
#' # View results
#' print(es)
#' plot(es)
#' }
#'
#' @export
dbevent <- function(
    outcome,
    data = NULL,
    conn = NULL,
    table = NULL,
    path = NULL,
    unit = NULL,
    time = NULL,
    treat = NULL,
    covariates = NULL,
    ref = -1L,
    leads = NULL,
    lags = NULL,
    vcov = "cluster",
    cluster = NULL,
    verbose = TRUE,
    ...
) {
  
  # -------------------------------------------------------------------------
  # 1. Setup connection
  # -------------------------------------------------------------------------
  db_setup <- setup_db_connection(
    conn = conn,
    table = table,
    data = data,
    path = path,
    caller = "dbevent"
  )
  conn <- db_setup$conn
  table_name <- db_setup$table_name
  backend_info <- detect_backend(conn)
  backend <- backend_info$name
  
  # -------------------------------------------------------------------------
  # 2. Validate required inputs
  # -------------------------------------------------------------------------
  if (missing(outcome) || is.null(outcome)) stop("'outcome' must be specified")
  if (is.null(unit)) stop("'unit' column must be specified")
  if (is.null(time)) stop("'time' column must be specified")
  if (is.null(treat)) stop("'treat' column must be specified")
  
  ref <- as.integer(ref)
  
  # -------------------------------------------------------------------------
  # 3. Validate data structure
  # -------------------------------------------------------------------------
  validation <- validate_event_data(
    conn = conn,
    table_name = table_name,
    unit = unit,
    time = time,
    treat = treat,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # 4. Detect or validate event time range
  # -------------------------------------------------------------------------
  event_range <- detect_event_range(
    conn = conn,
    table_name = table_name,
    unit = unit,
    time = time,
    treat = treat,
    leads = leads,
    lags = lags
  )
  
  if (verbose) {
    message(sprintf(
      "[dbevent] Event window: %d leads, %d lags",
      event_range$leads, event_range$lags
    ))
    message(sprintf(
      "[dbevent] Reference period(s): %s",
      paste(ref, collapse = ", ")
    ))
  }
  
  # -------------------------------------------------------------------------
  # 5. Create event study design table
  # -------------------------------------------------------------------------
  design <- create_event_table(
    conn = conn,
    table_name = table_name,
    unit = unit,
    time = time,
    treat = treat,
    leads = event_range$leads,
    lags = event_range$lags,
    ref = ref,
    backend = backend
  )
  
  # Ensure cleanup of temp table
  on.exit(
    try(DBI::dbRemoveTable(conn, design$table_name), silent = TRUE),
    add = TRUE
  )
  
  # -------------------------------------------------------------------------
  # 6. Build formula for dbreg
  # -------------------------------------------------------------------------
  # RHS: event dummies + optional covariates
  dummy_terms <- paste(design$dummy_cols, collapse = " + ")
  
  if (!is.null(covariates) && length(covariates) > 0) {
    covar_terms <- paste(covariates, collapse = " + ")
    rhs <- paste(dummy_terms, covar_terms, sep = " + ")
  } else {
    rhs <- dummy_terms
  }
  
  # FE: always unit + time
  fe_str <- paste(unit, time, sep = " + ")
  
  expanded_fml <- as.formula(paste(outcome, "~", rhs, "|", fe_str))
  
  if (verbose) {
    message(sprintf("[dbevent] Expanded formula: %s", deparse(expanded_fml)))
  }
  
  # -------------------------------------------------------------------------
  # 7. Call dbreg
  # -------------------------------------------------------------------------
  # Handle vcov: if "cluster" (the default), cluster on unit variable
  # Otherwise pass through the user-specified vcov
  if (is.character(vcov) && vcov == "cluster") {
    vcov_arg <- if (!is.null(cluster)) {
      cluster
    } else {
      as.formula(paste0("~", unit))
    }
  } else {
    vcov_arg <- vcov
  }
  
  dbreg_result <- dbreg(
    fml = expanded_fml,
    conn = conn,
    table = design$table_name,
    vcov = vcov_arg,
    verbose = verbose,
    ...
  )
  
  # -------------------------------------------------------------------------
  # 8. Extract and format event study coefficients
  # -------------------------------------------------------------------------
  event_coefs <- extract_event_coefs(
    model = dbreg_result,
    dummy_cols = design$dummy_cols,
    ref = ref
  )
  
  # -------------------------------------------------------------------------
  # 9. Build output object
  # -------------------------------------------------------------------------
  result <- structure(
    list(
      model = dbreg_result,
      coefs = event_coefs,
      event_time = event_range,
      ref = ref,
      validation = validation,
      call = match.call(),
      spec = list(outcome = outcome, unit = unit, time = time, treat = treat, 
                  covariates = covariates)
    ),
    class = "dbevent"
  )
  
  result
}


# =============================================================================
# Data Validation
# =============================================================================

#' Validate event study data structure
#'
#' Performs SQL-based checks to ensure data is suitable for event study:
#' binary treatment, absorbing treatment, unique unit-time pairs, etc.
#'
#' @param conn DBI connection
#' @param table_name Name of the table
#' @param unit Unit column name
#' @param time Time column name
#' @param treat Treatment column name
#' @param verbose Print messages
#'
#' @return List with validation results and summary statistics
#' @keywords internal
validate_event_data <- function(conn, table_name, unit, time, treat, verbose = TRUE) {
  checks <- list()
  
# -------------------------------------------------------------------------
  # Check 1: Treatment is binary (0/1)
  # -------------------------------------------------------------------------
  binary_check_sql <- glue::glue("
    SELECT COUNT(*) AS n_invalid
    FROM {table_name}
    WHERE {treat} NOT IN (0, 1) AND {treat} IS NOT NULL
  ")
  n_invalid <- DBI::dbGetQuery(conn, binary_check_sql)$n_invalid
  
  if (n_invalid > 0) {
    stop(sprintf(
      "Treatment column '%s' must be binary (0/1). Found %d invalid values.",
      treat, n_invalid
    ))
  }
  checks$binary_treatment <- TRUE
  
  # -------------------------------------------------------------------------
  # Check 2: Absorbing treatment (no reversals)
  # -------------------------------------------------------------------------
  reversal_sql <- glue::glue("
    WITH treated_periods AS (
      SELECT {unit}, MIN({time}) AS first_treat
      FROM {table_name}
      WHERE {treat} = 1
      GROUP BY {unit}
    )
    SELECT COUNT(*) AS n_reversals
    FROM {table_name} t
    INNER JOIN treated_periods tp ON t.{unit} = tp.{unit}
    WHERE t.{time} > tp.first_treat AND t.{treat} = 0
  ")
  n_reversals <- DBI::dbGetQuery(conn, reversal_sql)$n_reversals
  
  if (n_reversals > 0) {
    stop(sprintf(
      "Treatment must be absorbing (no reversals). Found %d cases where units returned to control after being treated.",
      n_reversals
    ))
  }
  checks$absorbing_treatment <- TRUE
  
  # -------------------------------------------------------------------------
  # Check 3: Panel uniqueness (no duplicate unit-time)
  # -------------------------------------------------------------------------
  dup_sql <- glue::glue("
    SELECT COUNT(*) AS n_dups
    FROM (
      SELECT {unit}, {time}, COUNT(*) AS n
      FROM {table_name}
      GROUP BY {unit}, {time}
      HAVING COUNT(*) > 1
    ) t
  ")
  n_dups <- DBI::dbGetQuery(conn, dup_sql)$n_dups
  
  if (n_dups > 0) {
    stop(sprintf(
      "Panel must have unique unit-time combinations. Found %d duplicate pairs.",
      n_dups
    ))
  }
  checks$panel_unique <- TRUE
  
  # -------------------------------------------------------------------------
  # Check 4: Always-treated units (warning only)
  # -------------------------------------------------------------------------
  always_treated_sql <- glue::glue("
    WITH unit_first_period AS (
      SELECT {unit}, MIN({time}) AS first_period
      FROM {table_name}
      GROUP BY {unit}
    ),
    unit_first_treat AS (
      SELECT {unit}, MIN({time}) AS first_treat
      FROM {table_name}
      WHERE {treat} = 1
      GROUP BY {unit}
    )
    SELECT COUNT(*) AS n_always_treated
    FROM unit_first_period ufp
    INNER JOIN unit_first_treat uft ON ufp.{unit} = uft.{unit}
    WHERE uft.first_treat = ufp.first_period
  ")
  n_always <- DBI::dbGetQuery(conn, always_treated_sql)$n_always_treated
  
  if (n_always > 0) {
    warning(sprintf(
      "%d unit(s) are treated from their first observed period (no pre-treatment data).",
      n_always
    ))
    checks$always_treated <- n_always
  } else {
    checks$always_treated <- 0
  }
  
  # -------------------------------------------------------------------------
  # Summary statistics
  # -------------------------------------------------------------------------
  summary_sql <- glue::glue("
    SELECT
      COUNT(DISTINCT {unit}) AS n_units,
      COUNT(DISTINCT {time}) AS n_periods,
      COUNT(*) AS n_obs,
      SUM(CASE WHEN {treat} = 1 THEN 1 ELSE 0 END) AS n_treated_obs
    FROM {table_name}
  ")
  summary_stats <- DBI::dbGetQuery(conn, summary_sql)
  
  # Count treated vs never-treated units
  treated_units_sql <- glue::glue("
    SELECT
      COUNT(DISTINCT CASE WHEN ever_treated = 1 THEN u END) AS n_treated_units,
      COUNT(DISTINCT CASE WHEN ever_treated = 0 THEN u END) AS n_never_treated
    FROM (
      SELECT {unit} AS u, MAX({treat}) AS ever_treated
      FROM {table_name}
      GROUP BY {unit}
    ) t
  ")
  treated_counts <- DBI::dbGetQuery(conn, treated_units_sql)
  
  if (verbose) {
    message("[dbevent] Data summary:")
    message(sprintf(
      "        - %s units, %s periods, %s observations",
      format(summary_stats$n_units, big.mark = ","),
      format(summary_stats$n_periods, big.mark = ","),
      format(summary_stats$n_obs, big.mark = ",")
    ))
    message(sprintf(
      "        - %s treated units, %s never-treated units",
      format(treated_counts$n_treated_units, big.mark = ","),
      format(treated_counts$n_never_treated, big.mark = ",")
    ))
    message(sprintf(
      "        - %s treated observations",
      format(summary_stats$n_treated_obs, big.mark = ",")
    ))
  }
  
  if (treated_counts$n_never_treated == 0) {
    warning("No never-treated units found. Using not-yet-treated as controls only.")
  }
  
  list(
    valid = TRUE,
    summary = summary_stats,
    treated_counts = treated_counts,
    checks = checks
  )
}


# =============================================================================
# Event Range Detection
# =============================================================================

#' Detect event time range from data
#'
#' Finds minimum and maximum event time (relative to treatment) in the data.
#'
#' @param conn DBI connection
#' @param table_name Table name
#' @param unit Unit column
#' @param time Time column
#' @param treat Treatment column
#' @param leads User-specified leads (NULL for auto-detect)
#' @param lags User-specified lags (NULL for auto-detect)
#'
#' @return List with leads and lags (positive integers)
#' @keywords internal
detect_event_range <- function(conn, table_name, unit, time, treat,
                                leads = NULL, lags = NULL) {
  
  range_sql <- glue::glue("
    WITH cohort_info AS (
      SELECT {unit} AS u, {time} AS t, {treat} AS tr,
        (SELECT MIN(t2.{time})
         FROM {table_name} t2
         WHERE t2.{unit} = t1.{unit} AND t2.{treat} = 1
        ) AS cohort
      FROM {table_name} t1
    )
    SELECT
      MIN(t - cohort) AS min_event_time,
      MAX(t - cohort) AS max_event_time
    FROM cohort_info
    WHERE cohort IS NOT NULL
  ")
  
  range_result <- DBI::dbGetQuery(conn, range_sql)
  
  # Use provided values or auto-detect
  actual_leads <- if (!is.null(leads)) {
    as.integer(leads)
  } else {
    as.integer(abs(range_result$min_event_time))
  }
  
  actual_lags <- if (!is.null(lags)) {
    as.integer(lags)
  } else {
    as.integer(range_result$max_event_time)
  }
  
  list(leads = actual_leads, lags = actual_lags)
}


# =============================================================================
# Event Table Creation
# =============================================================================

#' Create event study design table
#'
#' Creates a temporary table with cohort assignment, event time, and
#' indicator variables for each event time period.
#'
#' @param conn DBI connection
#' @param table_name Source table name
#' @param unit Unit column
#' @param time Time column
#' @param treat Treatment column
#' @param leads Number of pre-treatment periods
#' @param lags Number of post-treatment periods
#' @param ref Reference period(s) to exclude
#' @param backend Backend name from detect_backend()
#'
#' @return List with table_name and dummy_cols
#' @keywords internal
create_event_table <- function(conn, table_name, unit, time, treat,
                                leads, lags, ref, backend) {
  
  # Generate temp table name
  event_table <- temp_table_name("__dbevent_design", backend)
  
  # Build dummy column expressions
  dummy_cols <- character()
  dummy_exprs <- character()
  
  for (k in seq(-leads, lags)) {
    if (k %in% ref) next # skip reference period(s)
    
    # Name: es_m5 for k=-5, es_0 for k=0, es_p3 for k=3
    col_name <- if (k < 0) {
      paste0("es_m", abs(k))
    } else if (k == 0) {
      "es_0"
    } else {
      paste0("es_p", k)
    }
    
    expr <- glue::glue(
      "CASE WHEN __event_time = {k} AND __ever_treated = 1 THEN 1 ELSE 0 END AS {col_name}"
    )
    
    dummy_cols <- c(dummy_cols, col_name)
    dummy_exprs <- c(dummy_exprs, expr)
  }
  
  dummy_sql <- paste(dummy_exprs, collapse = ",\n      ")
  
  # Build the SELECT statement
  select_sql <- glue::glue("
    WITH cohort_info AS (
      SELECT
        *,
        (SELECT MIN(t2.{time})
         FROM {table_name} t2
         WHERE t2.{unit} = t1.{unit} AND t2.{treat} = 1
        ) AS __cohort
      FROM {table_name} t1
    ),
    with_event_time AS (
      SELECT *,
        CASE WHEN __cohort IS NOT NULL THEN 1 ELSE 0 END AS __ever_treated,
        CASE WHEN __cohort IS NOT NULL THEN {time} - __cohort ELSE NULL END AS __event_time
      FROM cohort_info
    )
    SELECT *,
      {dummy_sql}
    FROM with_event_time
  ")
  
  # Create the temp table
  create_temp_table_as(conn, event_table, select_sql, backend)
  
  list(
    table_name = event_table,
    dummy_cols = dummy_cols,
    leads = leads,
    lags = lags,
    ref = ref
  )
}


# =============================================================================
# Coefficient Extraction
# =============================================================================

#' Extract event study coefficients
#'
#' Parses coefficient names to event times, adds reference period,
#' and computes confidence intervals.
#'
#' @param model dbreg model object
#' @param dummy_cols Vector of dummy column names
#' @param ref Reference period(s)
#' @param level Confidence level for intervals
#'
#' @return Data frame with event_time, estimate, std.error, conf.low, conf.high
#' @keywords internal
extract_event_coefs <- function(model, dummy_cols, ref, level = 0.95) {
  
  coefs <- coef(model)
  vcov_mat <- vcov(model)
  
  # Parse event time from column name
  parse_event_time <- function(col) {
    if (grepl("^es_m", col)) {
      return(-as.integer(sub("es_m", "", col)))
    }
    if (col == "es_0") {
      return(0L)
    }
    if (grepl("^es_p", col)) {
      return(as.integer(sub("es_p", "", col)))
    }
    NA_integer_
  }
  
  # Get coefficients and SEs for dummy columns
  coef_names <- names(coefs)
  
  results <- data.frame(
    event_time = integer(),
    estimate = numeric(),
    std.error = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (col in dummy_cols) {
    # Find matching coefficient
    idx <- which(coef_names == col)
    
    if (length(idx) == 1) {
      k <- parse_event_time(col)
      est <- coefs[idx]
      se <- sqrt(vcov_mat[idx, idx])
      
      results <- rbind(results, data.frame(
        event_time = k,
        estimate = est,
        std.error = se,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Add reference period(s) with coefficient = 0
  for (r in ref) {
    results <- rbind(results, data.frame(
      event_time = r,
      estimate = 0,
      std.error = 0,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by event time
  results <- results[order(results$event_time), ]
  
  # Add confidence intervals
  z <- qnorm(1 - (1 - level) / 2)
  results$conf.low <- results$estimate - z * results$std.error
  results$conf.high <- results$estimate + z * results$std.error
  
  rownames(results) <- NULL
  results
}


# =============================================================================
# S3 Methods
# =============================================================================

#' @export
print.dbevent <- function(x, ...) {
  cat("Event Study Regression (dbevent)\n")
  cat("================================\n\n")
  
  cat(sprintf("Unit: %s, Time: %s, Treatment: %s\n",
              x$spec$unit, x$spec$time, x$spec$treat))
  cat(sprintf("Event window: [%d, %d]\n",
              -x$event_time$leads, x$event_time$lags))
  cat(sprintf("Reference period(s): %s\n\n",
              paste(x$ref, collapse = ", ")))
  
  # Summary stats
  cat(sprintf("N obs: %s, N units: %s, N periods: %s\n",
              format(x$validation$summary$n_obs, big.mark = ","),
              format(x$validation$summary$n_units, big.mark = ","),
              format(x$validation$summary$n_periods, big.mark = ",")))
  cat(sprintf("Treated units: %s, Never-treated: %s\n\n",
              format(x$validation$treated_counts$n_treated_units, big.mark = ","),
              format(x$validation$treated_counts$n_never_treated, big.mark = ",")))
  
  cat("Event Study Coefficients:\n")
  print_coefs <- x$coefs
  print_coefs$estimate <- sprintf("%.4f", print_coefs$estimate)
  print_coefs$std.error <- sprintf("%.4f", print_coefs$std.error)
  print_coefs$conf.low <- sprintf("%.4f", print_coefs$conf.low)
  print_coefs$conf.high <- sprintf("%.4f", print_coefs$conf.high)
  print(print_coefs, row.names = FALSE)
  
  invisible(x)
}

#' Extract coefficients from dbevent object
#' @param object A dbevent object
#' @param type Either "event" (default) for event study coefficients only,
#'   or "all" for all coefficients from the underlying model
#' @param ... Additional arguments (ignored)
#' @return Named numeric vector of coefficients
#' @export
#' @method coef dbevent
coef.dbevent <- function(object, type = c("event", "all"), ...) {
  type <- match.arg(type)
  if (type == "event") {
    setNames(object$coefs$estimate, object$coefs$event_time)
  } else {
    coef(object$model)
  }
}

#' @export
#' @method vcov dbevent
vcov.dbevent <- function(object, ...) {
  vcov(object$model)
}

#' @export
#' @method summary dbevent
summary.dbevent <- function(object, ...) {
  # Return a summary object with pre-trend test info
  cat("Event Study Summary\n")
  cat("===================\n\n")
  
  print(object)
  
  # Pre-trend coefficients (negative event times, excluding ref)
  pre_coefs <- object$coefs[object$coefs$event_time < 0 & 
                              !(object$coefs$event_time %in% object$ref), ]
  
  if (nrow(pre_coefs) > 0) {
    cat("\nPre-trend check:\n")
    # Simple check: are all pre-period CIs overlapping zero?
    pre_coefs$includes_zero <- pre_coefs$conf.low <= 0 & pre_coefs$conf.high >= 0
    n_significant <- sum(!pre_coefs$includes_zero)
    
    if (n_significant == 0) {
      cat("  All pre-treatment coefficients include zero (parallel trends supported)\n")
    } else {
      cat(sprintf("  %d of %d pre-treatment coefficients significantly different from zero\n",
                  n_significant, nrow(pre_coefs)))
    }
  }
  
  invisible(object)
}
