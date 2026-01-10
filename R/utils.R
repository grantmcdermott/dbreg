# Internal utility functions for dbreg
# These are general-purpose helpers used across multiple strategies

#' Convert internal interaction notation to standard R notation
#' @keywords internal
standardize_coef_names = function(x) gsub("_x_", ":", x, fixed = TRUE)

#' Generate coefficient table from estimates and vcov matrix
#' @keywords internal
gen_coeftable = function(betahat, vcov_mat, df_residual) {
  coefs = as.numeric(betahat)
  names(coefs) = standardize_coef_names(rownames(betahat))
  ses = sqrt(Matrix::diag(vcov_mat))
  tstats = coefs / ses
  pvals = 2 * pt(-abs(tstats), df_residual)
  cbind(estimate = coefs, std.error = ses, statistic = tstats, p.values = pvals)
}

#' Detect and handle collinearity in design matrix
#' 
#' Uses QR decomposition with pivoting to identify rank deficiency.
#' Returns reduced XtX/Xty matrices and lists of kept/dropped variables.
#' 
#' @keywords internal
detect_collinearity = function(XtX, Xty, tol = 1e-10, verbose = FALSE) {
  p = ncol(XtX)
  var_names = colnames(XtX)
  
  qr_decomp = qr(XtX, tol = tol)
  rank = qr_decomp$rank
  
  if (rank < p) {
    keep_idx = qr_decomp$pivot[seq_len(rank)]
    drop_idx = qr_decomp$pivot[(rank + 1):p]
    drop_names = var_names[drop_idx]
    keep_names = var_names[keep_idx]
    
    if (verbose) {
      message(sprintf(
        "[dbreg] %d variable(s) removed due to collinearity: %s",
        length(drop_names),
        paste(drop_names, collapse = ", ")
      ))
    }
    
    list(
      XtX = XtX[keep_idx, keep_idx, drop = FALSE],
      Xty = Xty[keep_idx, , drop = FALSE],
      keep_names = keep_names,
      drop_names = drop_names,
      collinear = TRUE
    )
  } else {
    list(
      XtX = XtX,
      Xty = Xty,
      keep_names = var_names,
      drop_names = character(0),
      collinear = FALSE
    )
  }
}

#' Solve linear system using Cholesky with QR fallback
#' @keywords internal
solve_with_fallback = function(XtX, Xty) {
  Rch = tryCatch(chol(XtX), error = function(e) NULL)
  if (is.null(Rch)) {
    # Cholesky failed, use QR fallback
    qr_decomp = qr(XtX)
    betahat = qr.solve(qr_decomp, Xty)
    XtX_inv = qr.solve(qr_decomp, diag(ncol(XtX)))
  } else {
    # Cholesky succeeded
    betahat = backsolve(Rch, forwardsolve(Matrix::t(Rch), Xty))
    XtX_inv = chol2inv(Rch)
  }
  dimnames(XtX_inv) = dimnames(XtX)
  list(betahat = betahat, XtX_inv = XtX_inv)
}


#' Set up database connection and data source
#'
#' Handles the precedence logic for data sources (table > data > path),
#' connection inference from tbl_lazy objects, and DuckDB registration.
#'
#' @param conn Database connection, or NULL to create a DuckDB connection

#' @param table Character table name or tbl_lazy object
#' @param data R data.frame to register with DuckDB
#' @param path Character path to file(s) on disk
#' @param caller Character string identifying the calling function for error messages
#'   and temp table naming (e.g., "dbreg" or "dbbinsreg")
#'
#' @return A list containing:
#'   \item{conn}{The database connection (created or provided)}
#'   \item{table_name}{The table name to query from}
#'   \item{from_statement}{SQL FROM clause (for dbreg-style queries)}
#'   \item{own_conn}{Logical, TRUE if we created the connection}
#'   \item{registered_table}{Name of registered table if data.frame was provided, else NULL}
#'   \item{is_duckdb}{Logical, TRUE if the connection is DuckDB}
#'
#' @keywords internal
setup_db_connection = function(conn, table, data, path, caller = "dbreg") {
  own_conn = FALSE
  registered_table = NULL
  is_duckdb = FALSE
  table_name = NULL
  from_statement = NULL


  # If table is tbl_lazy and conn is NULL, try to infer conn from the table
  if (is.null(conn) && inherits(table, "tbl_lazy")) {
    inferred_con = tryCatch(dbplyr::remote_con(table), error = function(e) NULL)
    if (!is.null(inferred_con) && DBI::dbIsValid(inferred_con)) {
      conn = inferred_con
    } else {
      stop(
        "Could not extract a valid database connection from the provided tbl_lazy. ",
        "The connection may be closed or invalid. ",
        "Either provide `conn` explicitly or ensure the tbl_lazy has an active connection."
      )
    }
  }

  # Create default connection if still NULL (for data.frame or path inputs)
  if (is.null(conn)) {
    conn = DBI::dbConnect(duckdb::duckdb(), shutdown = TRUE)
    own_conn = TRUE
    is_duckdb = TRUE
  } else {
    # Check if user-provided connection is DuckDB
    backend_info = detect_backend(conn)
    is_duckdb = (backend_info$name == "duckdb")
  }

  # Process data source with precedence: table > data > path
  if (!is.null(table)) {
    if (is.character(table)) {
      # Table name as string
      table_name = table
      from_statement = glue::glue("FROM {table}")
    } else if (inherits(table, "tbl_lazy")) {
      # Lazy table: render SQL as subquery
      rendered_sql = tryCatch(dbplyr::sql_render(table), error = function(e) NULL)
      if (is.null(rendered_sql)) {
        stop("Failed to render SQL for provided tbl_lazy.")
      }
      from_statement = paste0("FROM (", rendered_sql, ") AS lazy_subquery")
      table_name = paste0("(", rendered_sql, ") AS lazy_subquery")
      # Verify connection is valid
      if (!DBI::dbIsValid(conn)) {
        stop(
          "Could not obtain a valid database connection. ",
          "Either provide `conn` explicitly or ensure the tbl_lazy has an active connection."
        )
      }
    } else {
      stop("`table` must be character or tbl_lazy object.")
    }
  } else if (!is.null(data)) {
    if (!inherits(data, "data.frame")) {
      stop("`data` must be data.frame.")
    }
    # Coerce to base data.frame (handles tibbles, data.tables, etc.)
    data = as.data.frame(data)
    # Register with DuckDB (requires DuckDB connection)
    if (!is_duckdb) {
      stop("In-memory data frames are only supported with DuckDB connections. ",
           "Use `table` or `path` for other backends.")
    }
    temp_name = sprintf("__%s_%s",
                        caller,
                        gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
    duckdb::duckdb_register(conn, temp_name, data)
    registered_table = temp_name
    table_name = temp_name
    from_statement = paste("FROM", temp_name)
  } else if (!is.null(path)) {
    if (!is.character(path)) {
      stop("`path` must be character.")
    }
    # Handle path: check if it's already a read_* function call or raw path
    if (!(grepl("^read|^scan", path) && grepl("'", path))) {
      path = gsub('"', "'", path)
      from_statement = glue::glue("FROM '{path}'")
      table_name = glue::glue("'{path}'")
    } else {
      from_statement = glue::glue("FROM {path}")
      table_name = path
    }
  } else {
    stop("Provide one of `table`, `data`, or `path`.")
  }

  list(
    conn = conn,
    table_name = table_name,
    from_statement = from_statement,
    own_conn = own_conn,
    registered_table = registered_table,
    is_duckdb = is_duckdb
  )
}


#' Parse regression formula
#'
#' Parses a formula using the Formula package and extracts components needed
#' for regression: outcome variable, RHS variables, term labels, and fixed effects.
#'
#' @param fml A formula object (will be converted to Formula)
#'
#' @return A list containing:
#'   \item{fml}{The Formula object}
#'   \item{yvar}{Character, the outcome variable name}
#'   \item{xvars}{Character vector of unique variable names on RHS part 1}
#'   \item{term_labels}{Character vector of term labels (preserves interactions like "a:b")}
#'   \item{has_interactions}{Logical, TRUE if any interaction terms present}
#'   \item{fe}{Character vector of fixed effect variable names, or NULL if none}
#'
#' @keywords internal
#' @importFrom Formula Formula
parse_regression_formula = function(fml) {
  fml = Formula::Formula(fml)
  

  # Extract outcome variable (LHS)
  yvar = all.vars(stats::formula(fml, lhs = 1, rhs = 0))
  if (length(yvar) != 1) {
    stop("Exactly one outcome variable required.")
  }
  
  # Extract RHS part 1: variables and term structure
  rhs1 = stats::formula(fml, lhs = 0, rhs = 1)
  tt = stats::terms(rhs1)
  term_labels = attr(tt, "term.labels")
  xvars = all.vars(rhs1)  # unique variable names
  has_interactions = any(grepl(":", term_labels))
  
  if (!length(xvars)) {
    stop("No regressors on RHS.")
  }
  
  # Extract fixed effects (RHS part 2, after |)
  fe = if (length(fml)[2] > 1) {
    all.vars(stats::formula(fml, lhs = 0, rhs = 2))
  } else {
    NULL
  }
  
  list(
    fml = fml,
    yvar = yvar,
    xvars = xvars,
    term_labels = term_labels,
    has_interactions = has_interactions,
    fe = fe
  )
}


#' Parse variance-covariance and clustering arguments
#'
#' Parses the vcov and cluster arguments to determine the variance-covariance
#' type and extract any clustering variable.
#'
#' @param vcov Character string ("iid", "hc1") or formula for clustering (e.g., ~firm)
#' @param cluster Optional separate cluster argument (formula or character)
#' @param valid_types Character vector of valid vcov type strings. Default is
#'   c("iid", "hc1"). Case-insensitive matching is used.
#'
#' @return A list containing:
#'   \item{vcov_type}{Character string: "iid", "hc1", or "cluster"}
#'   \item{cluster_var}{Character name of cluster variable, or NULL if not clustered}
#'
#' @keywords internal
parse_vcov_args = function(vcov, cluster = NULL, valid_types = c("iid", "hc1")) {
  vcov_type = NULL
  cluster_var = NULL
  
 
  # Handle vcov: can be string or formula (for clustering)
  if (inherits(vcov, "formula")) {
    cluster_var = all.vars(vcov)
    if (length(cluster_var) != 1) {
      stop("Only single-variable clustering is currently supported")
    }
    vcov_type = "cluster"
  } else if (is.character(vcov)) {
    vcov_type = tolower(vcov[1])
    vcov_type = match.arg(vcov_type, valid_types)
  } else if (!is.null(vcov)) {
    stop("vcov must be a character string or a formula for clustering")
  }
  
  # Handle separate cluster argument (overrides vcov if provided)
  if (!is.null(cluster)) {
    if (inherits(cluster, "formula")) {
      cluster_var = all.vars(cluster)
      if (length(cluster_var) != 1) {
        stop("Only single-variable clustering is currently supported")
      }
    } else if (is.character(cluster)) {
      cluster_var = cluster[1]
    } else {
      stop("cluster must be a formula (e.g., ~firm) or character string")
    }
    vcov_type = "cluster"
  }
  
  list(
    vcov_type = vcov_type,
    cluster_var = cluster_var
  )
}
