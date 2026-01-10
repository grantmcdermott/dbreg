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

