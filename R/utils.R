# Internal utility functions for dbreg
# These are general-purpose helpers used across multiple strategies

#' Copy named values between environments
#'
#' @param source Source environment to read from
#' @param target Target environment to write to
#' @param keys Character vector of names to copy. If NULL, copies all.
#' @keywords internal
env2env = function(source, target, keys = NULL) {
  if (is.null(keys)) {
    keys = ls(source, all.names = TRUE)
  }
  for (nm in keys) {
    assign(nm, source[[nm]], envir = target)
  }
}

#' Convert internal interaction notation to standard R notation
#' @keywords internal
standardize_coef_names = function(x) gsub("_x_", ":", x, fixed = TRUE)

#' Format variance-covariance type labels for printing
#' @keywords internal
format_vcov_label = function(vcov_type, n_clusters = NULL) {
  switch(
    vcov_type,
    "iid" = "IID",
    "hc1" = "Heteroskedasticity-robust",
    "cluster" = if (!is.null(n_clusters)) sprintf("Clustered (%d clusters)", n_clusters) else "Clustered",
    "unconditional" = if (!is.null(n_clusters)) sprintf("Unconditional, clustered (%d clusters)", n_clusters) else "Unconditional",
    vcov_type
  )
}

#' Generate coefficient table from estimates and vcov matrix
#' @keywords internal
gen_coeftable = function(betahat, vcov_mat, df_residual) {
  coefs = as.numeric(betahat)
  names(coefs) = standardize_coef_names(rownames(betahat))
  ses = sqrt(Matrix::diag(vcov_mat))
  tstats = coefs / ses
  pvals = 2 * pt(-abs(tstats), df_residual)
  cbind(estimate = coefs, std.error = ses, statistic = tstats, p.value = pvals)
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
  rank = qr_decomp[["rank"]]
  
  if (rank < p) {
    keep_idx = qr_decomp[["pivot"]][seq_len(rank)]
    drop_idx = qr_decomp[["pivot"]][(rank + 1):p]
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
    if (!is.null(inferred_con) && dbIsValid(inferred_con)) {
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
    conn = dbConnect(duckdb(), shutdown = TRUE)
    own_conn = TRUE
    is_duckdb = TRUE
  } else {
    # Check if user-provided connection is DuckDB
    backend_info = detect_backend(conn)
    is_duckdb = (backend_info[["name"]] == "duckdb")
  }

  # Process data source with precedence: table > data > path
  if (!is.null(table)) {
    if (is.character(table)) {
      # Table name as string
      table_name = table
      from_statement = glue("FROM {table}")
    } else if (inherits(table, "tbl_lazy")) {
      # Lazy table: render SQL as subquery
      rendered_sql = tryCatch(dbplyr::sql_render(table), error = function(e) NULL)
      if (is.null(rendered_sql)) {
        stop("Failed to render SQL for provided tbl_lazy.")
      }
      from_statement = paste0("FROM (", rendered_sql, ") AS lazy_subquery")
      table_name = paste0("(", rendered_sql, ") AS lazy_subquery")
      # Verify connection is valid
      if (!dbIsValid(conn)) {
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
    duckdb_register(conn, temp_name, data)
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
      from_statement = glue("FROM '{path}'")
      table_name = glue("'{path}'")
    } else {
      from_statement = glue("FROM {path}")
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


#' Variables used by a formula
#'
#' Like [all.vars()], but only the variable arguments of `i()` terms count
#' (not, e.g., a `ref` value held in a variable).
#' @keywords internal
model_vars = function(x) {
  if (inherits(x, "Formula")) x = formula(x)
  walk = function(e) {
    if (is.name(e)) return(as.character(e))
    if (!is.call(e)) return(character())
    args = if (identical(e[[1]], as.name("i"))) {
      as.list(match.call(i_template, e))[c("factor_var", "var")]
    } else {
      as.list(e)[-1]
    }
    unlist(lapply(args, walk))
  }
  unique(walk(x))
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
parse_regression_formula = function(fml) {
  fml = Formula(fml)
  # Extract outcome variable (LHS)
  lhs = formula(fml, lhs = 1, rhs = 0)
  yvar = all.vars(lhs)
  if (length(yvar) != 1) {
    stop("Exactly one outcome variable required.")
  }
  check_simple_terms(paste(deparse(lhs[[2]]), collapse = " "), "outcome", allow_interactions = FALSE)
  
  # Extract RHS part 1: variables and term structure
  rhs1 = formula(fml, lhs = 0, rhs = 1)
  tt = terms(rhs1)
  term_labels = attr(tt, "term.labels")
  check_simple_terms(term_labels, "regressors")
  has_intercept = attr(tt, "intercept") == 1L
  xvars = model_vars(rhs1)  # unique variable names
  # i() terms need SQL expansion, like interactions
  has_i = vapply(term_labels, function(t) !is.null(parse_i_term(t)), logical(1))
  has_interactions = any(grepl(":", term_labels)) || any(has_i)
  
  if (!length(xvars)) {
    stop("No regressors on RHS.")
  }
  
  # Extract fixed effects (RHS part 2, after |)
  fe = if (length(fml)[2] > 1) {
    fe_rhs = formula(fml, lhs = 0, rhs = 2)
    check_simple_terms(attr(terms(fe_rhs), "term.labels"), "fixed effects", allow_interactions = FALSE)
    all.vars(fe_rhs)
  } else {
    NULL
  }
  
  list(
    fml = fml,
    yvar = yvar,
    xvars = xvars,
    term_labels = term_labels,
    has_interactions = has_interactions,
    has_intercept = has_intercept,
    fe = fe
  )
}


#' Release a connection set up by setup_db_connection()
#'
#' Unregisters any registered in-memory table and disconnects connections
#' that we created.
#' @keywords internal
release_db_setup = function(conn, own_conn, registered_table) {
  # Nothing of ours to release (e.g. a user connection, or setup never got
  # that far). Never throw: this runs in on.exit() and must not mask errors.
  if (!isTRUE(own_conn) && is.null(registered_table)) {
    return(invisible(NULL))
  }
  if (is.null(conn) || !isTRUE(tryCatch(dbIsValid(conn), error = function(e) FALSE))) {
    return(invisible(NULL))
  }
  if (!is.null(registered_table)) {
    try(duckdb_unregister(conn, registered_table), silent = TRUE)
  }
  if (isTRUE(own_conn)) {
    dbDisconnect(conn, shutdown = TRUE)
  }
  invisible(NULL)
}

# ---- SQL-safe identifiers ---------------------------------------------------
# Internally, all generated SQL uses identifiers that are valid unquoted on any
# backend. Model variables with other names (e.g. `Sepal.Length`, names with
# spaces, or reserved words) are renamed once at the source (see
# safe_source()) and mapped back to their display names in the final result
# (see restore_result_names()). Names starting with `dbreg_` are reserved for
# internal columns and aliases, so model variables with such names are renamed
# too.

# Common reserved words across backends (lower case)
sql_reserved = c(
  "all", "and", "as", "asc", "between", "by", "case", "cast", "check",
  "column", "create", "cross", "default", "delete", "desc", "distinct",
  "drop", "else", "end", "except", "exists", "false", "for", "foreign", "from",
  "full", "group", "having", "in", "inner", "insert", "intersect", "into", "is",
  "join", "left", "like", "limit", "natural", "not", "null", "offset", "on",
  "or", "order", "outer", "primary", "references", "right", "select", "table",
  "then", "to", "true", "union", "unique", "update", "user", "using", "values",
  "when", "where", "window", "with"
)

#' Does the backend fold the case of identifiers?
#'
#' Postgres folds unquoted identifiers to lower case, and Athena/Trino stores
#' and returns them in lower case. On these backends, safe identifiers must be
#' lower case (both to reference columns and to read results back by name).
#' @keywords internal
folds_identifier_case = function(conn) {
  detect_backend(conn)[["name"]] %in% c("postgres", "athena")
}

#' Is `x` usable as an unquoted SQL identifier?
#' @param fold_case Logical. Require lower case (see [folds_identifier_case()]).
#' @keywords internal
is_safe_identifier = function(x, fold_case = FALSE) {
  pattern = if (fold_case) "^[a-z_][a-z0-9_]*$" else "^[A-Za-z_][A-Za-z0-9_]*$"
  grepl(pattern, x) & !(tolower(x) %in% sql_reserved) & !startsWith(tolower(x), "dbreg_")
}

#' Make SQL-safe identifiers
#'
#' Leaves safe names unchanged; otherwise replaces invalid characters with `_`
#' (lower-casing on case-folding backends), prefixes names that would start
#' with a digit, are reserved words or start with the internal `dbreg_` prefix,
#' and de-duplicates against each other and
#' against `taken`. De-duplication ignores case, since most backends match
#' identifiers case-insensitively.
#' @inheritParams is_safe_identifier
#' @keywords internal
safe_identifiers = function(x, taken = character(), fold_case = FALSE) {
  keep = is_safe_identifier(x, fold_case)
  out = gsub("[^A-Za-z0-9_]", "_", x)
  if (fold_case) out = tolower(out)
  bad_start = !grepl("^[A-Za-z_]", out) | tolower(out) %in% sql_reserved | startsWith(tolower(out), "dbreg_")
  out[bad_start] = paste0("v_", out[bad_start])
  out[keep] = x[keep]
  gen = which(!keep)
  if (length(gen)) {
    # De-duplicate generated names (case-insensitively) against `taken`, the
    # kept names and each other in one pass; make.unique() leaves the earlier
    # (fixed) entries alone and suffixes later duplicates with _1, _2, ...
    fixed = tolower(c(taken, out[keep]))
    lc = tolower(out[gen])
    uniq = make.unique(c(fixed, lc), sep = "_")[-seq_along(fixed)]
    out[gen] = paste0(out[gen], substring(uniq, nchar(lc) + 1L))
  }
  out
}

# Rename variable symbols in an expression or formula (`map`: display -> safe).
# Function names (the head of a call) are left alone.
rename_expr_vars = function(e, map) {
  if (is.name(e)) {
    nm = as.character(e)
    return(if (nm %in% names(map)) as.name(map[[nm]]) else e)
  }
  if (is.call(e) && length(e) > 1L) {
    for (i in seq_along(e)[-1L]) {
      if (!is.null(e[[i]])) e[[i]] = rename_expr_vars(e[[i]], map)
    }
  }
  e
}

#' Give model variables SQL-safe names at the source
#'
#' Wraps `from_statement` in a subquery that selects `vars`, quoting (via
#' [glue::glue_sql()]) and aliasing any that are not safe identifiers.
#' If every variable is already safe, the source is returned unchanged.
#'
#' @return List with:
#'   - `from_statement`: the (possibly wrapped) FROM clause, and `table`, the
#'     same without the leading `FROM`;
#'   - `map`: display names indexed by their safe names (empty if nothing was
#'     renamed);
#'   - `to_safe()`, `to_display()`: map names (character vectors, `NULL`, or
#'     formulas for `to_safe()`) between the two. Identity if nothing was
#'     renamed.
#' @keywords internal
safe_source = function(conn, from_statement, vars) {
  vars = unique(vars[!is.na(vars) & nzchar(vars)])
  safe = safe_identifiers(vars, fold_case = folds_identifier_case(conn))
  renamed = safe != vars
  map = setNames(vars[renamed], safe[renamed])
  if (any(renamed)) {
    sel = vars
    # Quote the original names for this backend; keep the safe aliases unquoted
    # (quoting would make them case-sensitive on e.g. Postgres)
    sel[renamed] = glue_sql("{`vars[renamed]`} AS {DBI::SQL(safe[renamed])}", .con = conn)
    from_statement = paste0("FROM (SELECT ", paste(sel, collapse = ", "), " ", from_statement, ") AS dbreg_src")
  }
  lookup = function(x, from, to) {
    if (is.null(x) || !length(from)) return(x)
    i = match(x, from)
    ifelse(is.na(i), x, to[i])
  }
  list(
    from_statement = from_statement,
    table = sub("^FROM\\s+", "", from_statement),
    map = map,
    to_safe = function(x) {
      if (inherits(x, "formula")) {
        if (!length(map)) return(x)
        return(rename_expr_vars(formula(x), setNames(names(map), map)))
      }
      lookup(x, unname(map), names(map))
    },
    to_display = function(x) lookup(x, names(map), unname(map))
  )
}

#' Register display names for expanded factor columns
#'
#' Adds entries to `inputs$name_map` so that factor dummies named after their
#' (safe) variable and level map back to the display variable name plus the
#' original level text (e.g. `grptreated group`). Covers both R-side model
#' matrix names (`paste0(var, level)`) and the SQL-safe aliases generated by
#' [sql_model_matrix()].
#' @param factor_levels Named list of levels by (safe) variable name.
#' @param level_aliases Optional data frame of SQL aliases with their `var`
#'   and `level` (the `level_aliases` element of [sql_model_matrix()]).
#' @param i_columns Optional data frame of `i()` column aliases (the
#'   `i_columns` element of [sql_model_matrix()]). Their display names, e.g.
#'   `period::5:treat`, are also recorded in `inputs$i_columns` for
#'   `predict()`.
#' @keywords internal
register_factor_display = function(inputs, factor_levels = list(), level_aliases = NULL, i_columns = NULL) {
  map = inputs[["name_map"]]
  if (is.null(map)) map = setNames(character(), character())
  disp = function(v) coef_var_name(ifelse(v %in% names(map), map[v], v))
  vars = rep(names(factor_levels), lengths(factor_levels))
  lvls = unlist(lapply(factor_levels, as.character), use.names = FALSE)
  internal = c(paste0(vars, lvls), level_aliases[["alias"]])
  display = c(paste0(disp(vars), lvls), paste0(disp(level_aliases[["var"]]), level_aliases[["level"]]))
  new = setNames(display, internal)[internal != display]
  new = new[!duplicated(names(new)) & !names(new) %in% names(map)]
  if (NROW(i_columns)) {
    raw = function(v) ifelse(is.na(v), NA_character_, ifelse(v %in% names(map), map[v], v))
    i_display = data.frame(
      name = paste0(raw(i_columns[["var"]]), "::", i_columns[["level"]], ifelse(is.na(i_columns[["by"]]), "", paste0(":", raw(i_columns[["by"]])))),
      var = raw(i_columns[["var"]]),
      level = i_columns[["level"]],
      by = raw(i_columns[["by"]])
    )
    new = c(new, setNames(i_display[["name"]], i_columns[["alias"]]))
    i_display = rbind(inputs[["i_columns"]], i_display)
    inputs[["i_columns"]] = i_display[!duplicated(i_display[["name"]]), , drop = FALSE]
  }
  inputs[["name_map"]] = c(map, new)
  invisible(inputs)
}

#' Map internal (safe) names back to display names
#'
#' Maps each name via `map` (display names indexed by safe names), or else
#' splits it on the interaction separators (`:` and `_x_`) and maps each token.
#' @keywords internal
restore_names = function(x, map) {
  if (!length(map) || is.null(x) || !length(x)) {
    return(x)
  }
  out = vapply(x, function(s) {
    if (is.na(s)) return(s)
    if (s %in% names(map)) return(map[[s]])
    m = gregexpr(":|_x_", s)
    tokens = regmatches(s, m, invert = TRUE)[[1]]
    seps = regmatches(s, m)[[1]]
    tokens = ifelse(tokens %in% names(map), map[tokens], tokens)
    paste0(c(rbind(tokens, c(seps, ""))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  if (!is.null(names(x))) names(out) = names(x)
  out
}

# Variable names as they appear inside coefficient names: like model.matrix(),
# backtick names that aren't syntactic in R (e.g. `my grp`)
coef_var_name = function(v) ifelse(make.names(v) == v, v, paste0("`", v, "`"))

#' Map a result's internal names back to display names
#'
#' Applies `inputs$name_map` (see [safe_source()] and
#' [register_factor_display()]) to coefficient, variable and data names, and
#' restores the user's formula.
#' @keywords internal
restore_result_names = function(result, inputs) {
  map = inputs[["name_map"]]
  if (!length(map)) {
    return(result)
  }
  # Coefficient names backtick non-syntactic variable names (as in lm())
  coef_map = map
  is_var = names(coef_map) %in% names(inputs[["source_map"]])
  coef_map[is_var] = coef_var_name(coef_map[is_var])
  if (is.data.frame(result)) {
    names(result) = restore_names(names(result), map)
    return(result)
  }
  if (!is.null(result[["coeftable"]])) {
    rownames(result[["coeftable"]]) = restore_names(rownames(result[["coeftable"]]), coef_map)
  }
  if (!is.null(result[["vcov"]])) {
    dimnames(result[["vcov"]]) = lapply(dimnames(result[["vcov"]]), restore_names, map = coef_map)
  }
  for (f in c("xvars", "collin.var", "coef_names", "endogenous", "instruments")) {
    if (!is.null(result[[f]])) result[[f]] = restore_names(result[[f]], coef_map)
  }
  for (f in c("yvar", "fe", "weights")) {
    if (!is.null(result[[f]])) result[[f]] = restore_names(result[[f]], map)
  }
  if (!is.null(result[["n_fe_levels"]]) && !is.null(names(result[["n_fe_levels"]]))) {
    names(result[["n_fe_levels"]]) = restore_names(names(result[["n_fe_levels"]]), map)
  }
  if (is.data.frame(result[["data"]])) {
    names(result[["data"]]) = restore_names(names(result[["data"]]), map)
  }
  for (d in c("first_stage_f", "first_stage_wald")) {
    if (!is.null(names(result[["diagnostics"]][[d]]))) {
      names(result[["diagnostics"]][[d]]) = restore_names(names(result[["diagnostics"]][[d]]), coef_map)
    }
  }
  # The user's own formula (IV results store the structural formula)
  if (!is.null(result[["fml"]])) {
    fml = if (inherits(result[["fml"]], "Formula")) formula(result[["fml"]]) else result[["fml"]]
    result[["fml"]] = Formula(rename_expr_vars(fml, map))
  }
  result
}

#' Reject formula terms that cannot be expressed in SQL (yet)
#'
#' Terms must be plain variables, interactions of them (`a:b`) or (where
#' interactions are allowed) `i()` terms. Anything else, e.g. `log(x)`, `I(x^2)` or `factor(x)`, would otherwise be silently
#' reduced to its underlying variable by `all.vars()`.
#'
#' @param term_labels Term labels, e.g. from `attr(terms(f), "term.labels")`.
#' @param what Label for the error message.
#' @param allow_interactions Logical. Allow `a:b` terms.
#' @keywords internal
check_simple_terms = function(term_labels, what = "regressors", allow_interactions = TRUE) {
  is_simple = function(term) {
    # fixest-style i() terms are expanded by sql_model_matrix()
    if (allow_interactions && !is.null(parse_i_term(term))) return(TRUE)
    parts = if (allow_interactions) strsplit(term, ":", fixed = TRUE)[[1]] else term
    all(vapply(parts, function(x) {
      isTRUE(tryCatch(is.name(str2lang(x)), error = function(e) FALSE))
    }, logical(1)))
  }
  bad = term_labels[!vapply(term_labels, is_simple, logical(1))]
  if (allow_interactions && any(grepl("(^|:)i\\(", bad))) {
    stop(
      "i() terms can't be interacted with other variables yet: ",
      paste(bad[grepl("(^|:)i\\(", bad)], collapse = ", "),
      call. = FALSE
    )
  }
  if (length(bad)) {
    stop(
      "Transformed or function terms are not supported in the ", what, ": ",
      paste(bad, collapse = ", "), ". ",
      "Please create these variables in the data (or a database view) first.",
      call. = FALSE
    )
  }
  invisible(TRUE)
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

  validate_single_cluster = function(x) {
    if (length(x) != 1 || is.na(x) || !nzchar(x)) {
      stop("Only single-variable clustering is currently supported")
    }
    x
  }

  # Handle vcov: can be string or formula (for clustering)
  if (inherits(vcov, "formula")) {
    cluster_var = validate_single_cluster(all.vars(vcov))
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
      cluster_var = validate_single_cluster(all.vars(cluster))
    } else if (is.character(cluster)) {
      cluster_var = validate_single_cluster(cluster)
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

# =============================================================================
# SQL dialect helpers
# =============================================================================

#' Check if backend supports COUNT_BIG
#' 
#' SQL Server and Azure SQL use COUNT_BIG for large tables to avoid overflow.
#' 
#' @param conn A DBI database connection object.
#' @return Logical: TRUE if backend supports COUNT_BIG, FALSE otherwise.
#' @keywords internal
backend_supports_count_big = function(conn) {
  info = try(dbGetInfo(conn), silent = TRUE)
  if (inherits(info, "try-error")) {
    return(FALSE)
  }
  dbms = tolower(paste(info[["dbms.name"]], collapse = " "))
  grepl("sql server|azure sql|microsoft sql server", dbms)
}

#' Detect SQL backend from connection
#' 
#' Returns backend name and capabilities for SQL dialect handling.
#' 
#' @param conn A DBI database connection object.
#' @return List with `name` (character) and `supports_count_big` (logical).
#' @keywords internal
detect_backend = function(conn) {
  # First check connection class for DuckDB (dbms.name may be empty)
  if (inherits(conn, "duckdb_connection")) {
    return(list(name = "duckdb", supports_count_big = FALSE))
  }
  if (inherits(conn, "BigQueryConnection")) {
    return(list(name = "bigquery", supports_count_big = FALSE))
  }

  
  info = try(dbGetInfo(conn), silent = TRUE)
  if (inherits(info, "try-error")) {
    return(list(name = "unknown", supports_count_big = FALSE))
  }
  dbms = tolower(paste(info[["dbms.name"]], collapse = " "))
  list(
    name = if (grepl("duckdb", dbms)) {
      "duckdb"
    } else if (grepl("sql server|azure sql|microsoft sql server", dbms)) {
      "sqlserver"
    } else if (grepl("postgres", dbms)) {
      "postgres"
    } else if (grepl("sqlite", dbms)) {
      "sqlite"
    } else if (grepl("mysql|mariadb", dbms)) {
      "mysql"
    } else if (grepl("spark", dbms)) {
      "spark"
    } else if (grepl("athena|presto|trino", dbms)) {
      "athena"
    } else {
      "other"
    },
    supports_count_big = grepl(
      "sql server|azure sql|microsoft sql server",
      dbms
    )
  )
}

#' Cast to double precision
#'
#' `FLOAT` is single precision on DuckDB, MySQL, Spark and Athena (`REAL`),
#' which limits moment accuracy to ~1e-7. Use each backend's 8-byte type.
#' @keywords internal
sql_cast_double = function(sql, conn) {
  type = switch(
    detect_backend(conn)[["name"]],
    postgres = "DOUBLE PRECISION",
    sqlserver = "FLOAT",
    bigquery = "FLOAT64",
    "DOUBLE"
  )
  sprintf("CAST(%s AS %s)", sql, type)
}

#' Generate SQL COUNT expression with proper casting
#' 
#' Returns an aliased COUNT expression that handles:
#' - COUNT_BIG for SQL Server (avoids int overflow on large tables)
#' - CAST to BIGINT for other backends
#' - DISTINCT counting when needed
#' 
#' @param conn Database connection (used to detect backend)
#' @param alias Column alias for the count result
#' @param expr Expression to count (default "*")
#' @param distinct Logical; if TRUE, count distinct values
#' @return SQL expression string like "CAST(COUNT(*) AS BIGINT) AS n"
#' @keywords internal
sql_count = function(conn, alias, expr = "*", distinct = FALSE) {
  bd = detect_backend(conn)
  use_count_big = bd[["supports_count_big"]]
  if (distinct) {
    glue(
      "{if (use_count_big) paste0('COUNT_BIG(DISTINCT ', expr, ')') else paste0('CAST(COUNT(DISTINCT ', expr, ') AS BIGINT)')} AS {alias}"
    )
  } else {
    if (use_count_big) {
      glue("COUNT_BIG({expr}) AS {alias}")
    } else {
      glue("CAST(COUNT({expr}) AS BIGINT) AS {alias}")
    }
  }
}

#' Get simple COUNT expression for backend
#' 
#' Returns just the count expression without alias (for use in aggregations).
#' 
#' @param backend Backend name from detect_backend()
#' @return SQL expression: "COUNT_BIG(*)" for SQL Server, "COUNT(*)" otherwise
#' @keywords internal
sql_count_expr = function(backend) {
  if (backend == "sqlserver") "COUNT_BIG(*)" else "COUNT(*)"
}

#' Get NTILE window function expression
#' @param x_name Column name to partition by
#' @param n_bins Number of bins
#' @return SQL NTILE expression
#' @keywords internal
sql_ntile = function(x_name, n_bins) {
  glue("NTILE({n_bins}) OVER (ORDER BY {x_name})")
}

#' Get backend-specific RANDOM() expression
#' 
#' Different SQL backends use different syntax for random number generation.
#' 
#' @param backend Backend name from detect_backend()
#' @return SQL expression for random value (e.g., "RANDOM()", "NEWID()", "RAND()")
#' @keywords internal
sql_random = function(backend) {
  switch(backend,
    "duckdb" = "RANDOM()",
    "sqlserver" = "NEWID()",
    "postgres" = "RANDOM()",
    "sqlite" = "RANDOM()",
    "mysql" = "RAND()",
    "RANDOM()"  # default fallback
  )
}

#' Build efficient sample query (backend-specific)
#' 
#' Uses native SAMPLE/TABLESAMPLE when available, falls back to ORDER BY RANDOM().
#' 
#' @param select_clause The SELECT clause (e.g., "*" or "col1, col2")
#' @param table_name Table to sample from
#' @param where_clause WHERE clause without "WHERE" keyword (or NULL)
#' @param n Number of rows to sample
#' @param backend Backend name from detect_backend()
#' @return Complete SQL query for sampling
#' @keywords internal
sql_sample = function(select_clause, table_name, where_clause, n, backend) {
  where_sql = if (!is.null(where_clause)) paste("WHERE", where_clause) else ""
  
  if (backend == "duckdb") {
    # DuckDB: efficient reservoir sampling
    glue("SELECT {select_clause} FROM {table_name} {where_sql} USING SAMPLE {n} ROWS")
  } else if (backend == "spark") {
    # Spark SQL: TABLESAMPLE with ROWS
    glue("SELECT {select_clause} FROM {table_name} TABLESAMPLE ({n} ROWS) {where_sql}")
  } else if (backend %in% c("athena", "presto", "trino")) {
    # Athena/Presto/Trino: TABLESAMPLE BERNOULLI only supports percentages
    # Fall back to ORDER BY RANDOM() for exact row counts
    glue("SELECT {select_clause} FROM {table_name} {where_sql} ORDER BY RANDOM() LIMIT {n}")
  } else if (backend == "postgres") {
    # PostgreSQL: TABLESAMPLE BERNOULLI only supports percentages
    random_expr = sql_random(backend)
    glue("SELECT {select_clause} FROM {table_name} {where_sql} ORDER BY {random_expr} LIMIT {n}")
  } else if (backend == "sqlserver") {
    # SQL Server: TABLESAMPLE is block-level, not row-level; use TOP with NEWID()
    glue("SELECT TOP {n} {select_clause} FROM {table_name} {where_sql} ORDER BY NEWID()")
  } else {
    # SQLite, MySQL, others: ORDER BY RANDOM() LIMIT n
    random_expr = sql_random(backend)
    glue("SELECT {select_clause} FROM {table_name} {where_sql} ORDER BY {random_expr} LIMIT {n}")
  }
}

#' Apply row limit to a SQL query (backend-specific)
#' 
#' SQL Server uses TOP after SELECT, others use LIMIT at end.
#' 
#' @param query The SQL query string
#' @param n Number of rows to limit to
#' @param backend Backend name from detect_backend()
#' @return SQL query with appropriate LIMIT/TOP clause
#' @keywords internal
sql_limit = function(query, n, backend) {
  if (backend == "sqlserver") {
    # SQL Server uses TOP n after SELECT
    sub("^SELECT", paste("SELECT TOP", n), query, ignore.case = TRUE)
  } else {
    # Others use LIMIT at the end
    paste(query, "LIMIT", n)
  }
}

#' Generate a temp table name with optional SQL Server prefix
#' 
#' SQL Server uses # prefix for local temp tables.
#' 
#' @param base_name Base name for the temp table
#' @param backend Backend name from detect_backend()
#' @return Temp table name (with # prefix for SQL Server)
#' @keywords internal
temp_table_name = function(base_name, backend) {
  if (backend == "sqlserver") {
    paste0("#", base_name)
  } else {
    base_name
  }
}

#' Create a temp table from a SELECT query
#' 
#' Handles SQL dialect differences:
#' - SQL Server: SELECT ... INTO #table FROM (SELECT ...) AS __subq
#' - Others: CREATE TEMPORARY TABLE name AS SELECT ...
#' 
#' @param conn Database connection
#' @param table_name Name for the temp table
#' @param select_sql The SELECT statement (without CREATE TABLE)
#' @param backend Backend name from detect_backend()
#' @keywords internal
create_temp_table_as = function(conn, table_name, select_sql, backend) {
  if (backend == "sqlserver") {
    # SQL Server: SELECT ... INTO #table FROM ...
    sql = sub("^SELECT", paste0("SELECT * INTO ", table_name, " FROM (SELECT"), 
               select_sql, ignore.case = TRUE)
    sql = paste0(sql, ") AS __subq")
    dbExecute(conn, sql)
  } else if (backend == "spark") {
    # Spark/Databricks: CREATE OR REPLACE TEMPORARY TABLE ... (session temp
    # tables can linger across scopes, so replace rather than fail)
    sql = glue("CREATE OR REPLACE TEMPORARY TABLE {table_name} AS {select_sql}")
    dbExecute(conn, sql)
  } else {
    # Standard SQL: CREATE TEMPORARY TABLE name AS SELECT ...
    sql = glue("CREATE TEMPORARY TABLE {table_name} AS {select_sql}")
    dbExecute(conn, sql)
  }
}

#' Drop a table if it exists
#'
#' Handles SQL dialect differences for temporary-table cleanup.
#'
#' @param conn Database connection
#' @param table_name Name of the table to drop
#' @param backend Backend name from detect_backend()
#' @keywords internal
drop_table_if_exists = function(conn, table_name, backend) {
  if (backend == "sqlserver") {
    sql = glue("IF OBJECT_ID('tempdb..{table_name}') IS NOT NULL DROP TABLE {table_name}")
  } else if (backend == "spark") {
    sql = glue("DROP TEMPORARY TABLE IF EXISTS {table_name}")
  } else {
    sql = glue("DROP TABLE IF EXISTS {table_name}")
  }
  tryCatch(dbExecute(conn, sql), error = function(e) NULL)
}
