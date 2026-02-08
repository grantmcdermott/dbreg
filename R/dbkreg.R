#' Kernel regression on a database backend
#'
#' @md
#' @description
#' Fits local constant (Nadaraya-Watson) or local linear kernel regressions
#' by computing weighted sufficient statistics in SQL. The evaluation grid
#' is user-controlled, so the returned object contains only grid points
#' (not the full dataset).
#'
#' @param fml A \code{\link[stats]{formula}} with running variables on the RHS.
#' Interactions, controls, and fixed effects are not yet supported.
#' @inheritParams dbreg
#' @param eval Evaluation mode. One of `"grid"` (default), `"points"` (user-
#' supplied grid), or `"data"` (unique x values from the data; 1D only).
#' @param grid Evaluation points used when `eval = "points"`. For 1D, a numeric
#' vector is allowed. For multi-D, supply a data frame with one column per RHS
#' variable.
#' @param n_eval Integer number of evaluation points when `eval = "grid"`.
#' @param grid_method How to construct the grid when `eval = "grid"`. `"quantile"`
#' uses sample quantiles and `"equal"` uses evenly spaced points between min/max.
#' @param randcut Optional sampling fraction in (0,1]. If provided, the kernel
#' regression is estimated on a random subset of the data of size
#' `ceiling(randcut * N)`. Useful for quick approximations on very large datasets.
#' @param bandwidth Positive numeric bandwidth. Can be a scalar (recycled across
#' dimensions) or a vector with length equal to the number of RHS variables.
#' @param kernel Kernel name. One of `"epanechnikov"` (default), `"uniform"`,
#' `"biweight"`, or `"triweight"`.
#' @param degree Polynomial degree. `0` for local constant, `1` for local linear.
#' @param ci Logical. Compute pointwise confidence intervals? Default is `FALSE`.
#' @param level Confidence level for intervals. Default is `0.95`.
#' @param verbose Logical. Print progress messages? Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of class "dbkreg" containing:
#' \describe{
#'   \item{grid}{Data frame of evaluation points and fitted values.}
#'   \item{opt}{List of options used (kernel, bandwidth, degree, etc.).}
#' }
#'
#' @export
dbkreg = function(
  fml,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  eval = c("grid", "points", "data"),
  grid = NULL,
  n_eval = 200,
  grid_method = c("quantile", "equal"),
  randcut = NULL,
  bandwidth,
  kernel = c("epanechnikov", "uniform", "biweight", "triweight"),
  degree = c(0, 1),
  ci = FALSE,
  level = 0.95,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  
  verbose = isTRUE(verbose)
  eval = match.arg(eval)
  grid_method = match.arg(grid_method)
  kernel = match.arg(kernel)
  if (!is.numeric(degree) || length(degree) != 1 || !(degree %in% c(0, 1))) {
    stop("Argument `degree` must be 0 (local constant) or 1 (local linear).")
  }
  degree = as.integer(degree)
  
  if (!is.logical(ci) || length(ci) != 1) {
    stop("Argument `ci` must be TRUE or FALSE.")
  }
  ci = isTRUE(ci)
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("Argument `level` must be in (0, 1).")
  }
  
  if (!is.null(randcut)) {
    if (!is.numeric(randcut) || length(randcut) != 1 || randcut <= 0 || randcut > 1) {
      stop("`randcut` must be NULL or a numeric value in (0, 1].")
    }
  }
  
  # Parse formula
  fml_parsed = parse_regression_formula(fml)
  if (fml_parsed$has_interactions) {
    stop("dbkreg does not support interaction terms.")
  }
  if (!is.null(fml_parsed$fe)) {
    stop("dbkreg does not support fixed effects.")
  }
  y_name = fml_parsed$yvar
  xvars = fml_parsed$xvars
  if (!length(xvars)) {
    stop("No running variables provided on RHS.")
  }
  d = length(xvars)
  
  if (eval == "grid" && d > 1) {
    stop("eval = 'grid' is only supported for 1D. Use eval = 'points' with a grid data frame.")
  }
  if (eval == "data" && d > 1) {
    stop("eval = 'data' is only supported for 1D. Use eval = 'points' with a grid data frame.")
  }
  
  # Bandwidth
  if (missing(bandwidth)) {
    stop("Argument `bandwidth` is required.")
  }
  if (!is.numeric(bandwidth) || length(bandwidth) < 1 || any(bandwidth <= 0)) {
    stop("Argument `bandwidth` must be positive.")
  }
  if (length(bandwidth) == 1) {
    bandwidth = rep(bandwidth, d)
  } else if (length(bandwidth) != d) {
    stop("`bandwidth` must be length 1 or match the number of RHS variables.")
  }
  
  # Grid settings
  if (eval == "points") {
    if (d == 1 && is.numeric(grid)) {
      grid_df = data.frame(x = as.numeric(grid))
      names(grid_df)[1] = xvars[1]
    } else if (is.data.frame(grid)) {
      if (!all(xvars %in% names(grid))) {
        stop("`grid` must contain columns for all RHS variables.")
      }
      grid_df = grid[, xvars, drop = FALSE]
    } else {
      stop("`grid` must be numeric (1D) or a data frame with RHS columns (multi-D).")
    }
  } else {
    if (!is.numeric(n_eval) || length(n_eval) != 1 || n_eval < 1) {
      stop("`n_eval` must be a positive integer.")
    }
    n_eval = as.integer(n_eval)
    grid_df = NULL
  }
  
  # Set up database connection and data source
  db_setup = setup_db_connection(conn, table, data, path, caller = "dbkreg")
  conn = db_setup$conn
  conn_managed = db_setup$own_conn
  registered_table = db_setup$registered_table
  from_statement = db_setup$from_statement
  
  # Detect backend
  backend_info = detect_backend(conn)
  backend = backend_info$name
  
  # Cleanup connection and registered table
  cleanup = function() {
    if (conn_managed && dbIsValid(conn)) {
      dbDisconnect(conn, shutdown = TRUE)
    }
  }
  on.exit(cleanup(), add = TRUE)
  
  cleanup_registered = function() {
    if (!is.null(registered_table) && dbIsValid(conn)) {
      try(duckdb_unregister(conn, registered_table), silent = TRUE)
    }
  }
  on.exit(cleanup_registered(), add = TRUE)
  
  # Build base FROM clause with missingness filter
  where_terms = c(y_name, xvars)
  where_clause = paste(where_terms, "IS NOT NULL", collapse = " AND ")
  from_statement = add_where_clause(from_statement, where_clause)
  
  # Optional sampling (subsets data used for estimation)
  if (!is.null(randcut) && randcut < 1) {
    count_sql = glue("SELECT {sql_count(conn, 'n')} {from_statement}")
    n_total = dbGetQuery(conn, count_sql)$n
    if (n_total <= 0) {
      stop("No observations available for sampling.")
    }
    n_sample = max(1L, as.integer(ceiling(n_total * randcut)))
    if (verbose) {
      message(sprintf("[dbkreg] Sampling %s / %s rows (randcut = %.3f)",
                      prettyNum(n_sample, big.mark = ","),
                      prettyNum(n_total, big.mark = ","),
                      randcut))
    }
    sample_tbl = sample_from_statement(
      conn = conn,
      backend = backend,
      from_statement = from_statement,
      n = n_sample
    )
    from_statement = glue("FROM {sample_tbl}")
  }
  
  # Build evaluation grid values if needed (1D only)
  if (eval != "points") {
    grid_vals = build_grid_values(
      eval = eval,
      grid_method = grid_method,
      n_eval = n_eval,
      x = xvars[1],
      y = y_name,
      data = data,
      conn = conn,
      from_statement = from_statement,
      backend = backend,
      verbose = verbose
    )
    grid_vals = as.numeric(grid_vals)
    grid_vals = grid_vals[!is.na(grid_vals)]
    if (!length(grid_vals)) {
      stop("No valid grid values available.")
    }
    grid_df = data.frame(x = grid_vals)
    names(grid_df)[1] = xvars[1]
  }
  
  # Warn if eval = "data" produces a very large grid
  if (eval == "data" && nrow(grid_df) > 5000 && verbose) {
    message("[dbkreg] Large evaluation grid (n = ", nrow(grid_df), "). Consider supplying `grid` explicitly.")
  }
  
  # Build grid source (CTE or temp table)
  grid_source = build_grid_source(
    conn = conn,
    backend = backend,
    grid_df = grid_df,
    xvars = xvars,
    cte_max = 500L
  )
  
  # Base CTE
  base_cols = paste(c(xvars, y_name), collapse = ", ")
  base_cte = glue("base AS (SELECT {base_cols} {from_statement})")
  
  # Kernel weight expression
  w_sql = kernel_weight_sql_mdim(xvars = xvars, bandwidth = bandwidth, kernel = kernel)
  
  # Join condition for compact support
  join_terms = vapply(seq_along(xvars), function(i) {
    h = format_numeric_sql(bandwidth[i])
    glue("t.{xvars[i]} BETWEEN g.{xvars[i]} - {h} AND g.{xvars[i]} + {h}")
  }, character(1))
  join_sql = paste(join_terms, collapse = " AND ")
  
  # Build SQL select terms
  select_terms = c(
    paste0("g.", xvars, " AS ", xvars),
    glue("SUM({w_sql}) AS S0"),
    glue("SUM(({w_sql}) * t.{y_name}) AS T0")
  )
  
  if (ci) {
    select_terms = c(
      select_terms,
      glue("SUM(({w_sql}) * t.{y_name} * t.{y_name}) AS T2"),
      glue("{sql_count(conn, 'n_win')}")
    )
  }
  
  if (degree == 1) {
    if (d == 1) {
      u = glue("t.{xvars[1]} - g.{xvars[1]}")
      select_terms = c(
        select_terms,
        glue("SUM(({w_sql}) * ({u})) AS S1"),
        glue("SUM(({w_sql}) * ({u}) * ({u})) AS S2"),
        glue("SUM(({w_sql}) * ({u}) * t.{y_name}) AS T1")
      )
    } else {
      for (i in seq_len(d)) {
        ui = glue("t.{xvars[i]} - g.{xvars[i]}")
        select_terms = c(
          select_terms,
          glue("SUM(({w_sql}) * ({ui})) AS S{i}"),
          glue("SUM(({w_sql}) * ({ui}) * t.{y_name}) AS T{i}")
        )
      }
      for (i in seq_len(d)) {
        ui = glue("t.{xvars[i]} - g.{xvars[i]}")
        for (j in i:d) {
          uj = glue("t.{xvars[j]} - g.{xvars[j]}")
          select_terms = c(
            select_terms,
            glue("SUM(({w_sql}) * ({ui}) * ({uj})) AS S{i}_{j}")
          )
        }
      }
    }
  }
  
  grid_ref = grid_source$grid_ref
  with_sql = paste(
    "WITH",
    paste(c(base_cte, grid_source$grid_cte), collapse = ",\n"),
    sep = "\n"
  )
  
  order_sql = paste(paste0("g.", xvars), collapse = ", ")
  
  sql = glue(
    "{with_sql}
     SELECT {paste(select_terms, collapse = ', ')}
     FROM {grid_ref} g
     JOIN base t
       ON {join_sql}
     GROUP BY {paste(paste0('g.', xvars), collapse = ', ')}
     ORDER BY {order_sql}"
  )
  
  if (verbose) {
    message("[dbkreg] Executing kernel regression query")
  }
  
  sums = dbGetQuery(conn, sql)
  
  # Compute fitted values
  if (degree == 0) {
    fit = sums$T0 / sums$S0
    fit[!is.finite(fit)] = NA_real_
  } else if (d == 1) {
    denom = sums$S0 * sums$S2 - sums$S1 * sums$S1
    fit = (sums$S2 * sums$T0 - sums$S1 * sums$T1) / denom
    fit[!is.finite(fit)] = NA_real_
  } else {
    fit = numeric(nrow(sums))
    fit[] = NA_real_
    for (r in seq_len(nrow(sums))) {
      row = sums[r, , drop = FALSE]
      s0 = row$S0
      s_vec = vapply(seq_len(d), function(i) row[[paste0("S", i)]], numeric(1))
      t_vec = vapply(seq_len(d), function(i) row[[paste0("T", i)]], numeric(1))
      s_mat = matrix(0, d, d)
      for (i in seq_len(d)) {
        for (j in i:d) {
          val = row[[paste0("S", i, "_", j)]]
          s_mat[i, j] = val
          s_mat[j, i] = val
        }
      }
      XWX = rbind(c(s0, s_vec), cbind(s_vec, s_mat))
      XWy = c(row$T0, t_vec)
      beta = tryCatch(solve(XWX, XWy), error = function(e) NULL)
      if (!is.null(beta)) {
        fit[r] = beta[1]
      }
    }
  }
  
  # Confidence intervals
  if (ci) {
    alpha = 1 - level
    crit = stats::qnorm(1 - alpha / 2)
    se = rep(NA_real_, nrow(sums))
    if (degree == 0) {
      sse = sums$T2 - 2 * fit * sums$T0 + fit * fit * sums$S0
      df = sums$n_win - 1
      sigma2 = sse / df
      se = sqrt(sigma2 / sums$S0)
    } else if (d == 1) {
      sse = sums$T2 - 2 * fit * sums$T0 + fit * fit * sums$S0
      df = sums$n_win - 2
      sigma2 = sse / df
      denom = sums$S0 * sums$S2 - sums$S1 * sums$S1
      se = sqrt(sigma2 * (sums$S2 / denom))
    } else {
      for (r in seq_len(nrow(sums))) {
        row = sums[r, , drop = FALSE]
        s0 = row$S0
        s_vec = vapply(seq_len(d), function(i) row[[paste0("S", i)]], numeric(1))
        t_vec = vapply(seq_len(d), function(i) row[[paste0("T", i)]], numeric(1))
        s_mat = matrix(0, d, d)
        for (i in seq_len(d)) {
          for (j in i:d) {
            val = row[[paste0("S", i, "_", j)]]
            s_mat[i, j] = val
            s_mat[j, i] = val
          }
        }
        XWX = rbind(c(s0, s_vec), cbind(s_vec, s_mat))
        XWy = c(row$T0, t_vec)
        beta = tryCatch(solve(XWX, XWy), error = function(e) NULL)
        if (is.null(beta)) {
          next
        }
        sse = row$T2 - 2 * drop(t(beta) %*% XWy) + drop(t(beta) %*% XWX %*% beta)
        df = row$n_win - (d + 1)
        if (df <= 0) {
          next
        }
        sigma2 = sse / df
        XWX_inv = tryCatch(solve(XWX), error = function(e) NULL)
        if (is.null(XWX_inv)) {
          next
        }
        se[r] = sqrt(sigma2 * XWX_inv[1, 1])
      }
    }
    se[!is.finite(se)] = NA_real_
    lwr = fit - crit * se
    upr = fit + crit * se
  }
  
  grid_df = sums[, xvars, drop = FALSE]
  grid_df$fit = as.numeric(fit)
  if (ci) {
    grid_df$se = se
    grid_df$lwr = lwr
    grid_df$upr = upr
  }
  
  out = list(
    grid = grid_df,
    opt = list(
      x_vars = xvars,
      x_var = if (d == 1) xvars[1] else NULL,
      y_var = y_name,
      kernel = kernel,
      bandwidth = bandwidth,
      degree = degree,
      eval = eval,
      grid_method = if (eval == "grid") grid_method else NULL,
      n_eval = if (eval == "grid") n_eval else NULL,
      ci = ci,
      level = level
    )
  )
  class(out) = "dbkreg"
  out
}

#' Add a WHERE clause to a FROM statement safely
#' @keywords internal
add_where_clause = function(from_statement, where_clause) {
  if (grepl("WHERE|LIMIT|ORDER\\s+BY|GROUP\\s+BY|HAVING", from_statement, ignore.case = TRUE)) {
    from_statement = glue("FROM (SELECT * {from_statement}) AS subq")
  }
  glue("{from_statement} WHERE {where_clause}")
}

#' Build evaluation grid values (1D)
#' @keywords internal
build_grid_values = function(
  eval,
  grid_method,
  n_eval,
  x,
  y,
  data,
  conn,
  from_statement,
  backend,
  verbose
) {
  if (eval == "data") {
    sql = glue("SELECT DISTINCT {x} AS x0 {from_statement} ORDER BY {x}")
    return(dbGetQuery(conn, sql)$x0)
  }
  
  # eval = "grid"
  if (n_eval == 1) {
    probs = 0.5
  } else {
    probs = seq(0, 1, length.out = n_eval)
  }
  
  if (grid_method == "equal") {
    sql = glue("SELECT MIN({x}) AS xmin, MAX({x}) AS xmax {from_statement}")
    xr = dbGetQuery(conn, sql)
    if (n_eval == 1) {
      return(mean(c(xr$xmin, xr$xmax)))
    }
    return(seq(xr$xmin, xr$xmax, length.out = n_eval))
  }
  
  # grid_method == "quantile"
  if (!is.null(data)) {
    keep = !is.na(data[[x]]) & !is.na(data[[y]])
    return(as.numeric(stats::quantile(data[[x]][keep], probs = probs, type = 1, na.rm = TRUE)))
  }
  
  # Use row-number quantiles from SQL
  count_sql = glue("SELECT {sql_count(conn, 'n')} {from_statement}")
  n_total = dbGetQuery(conn, count_sql)$n
  if (n_total <= 0) {
    stop("No non-missing observations available for grid construction.")
  }
  
  idx = floor((n_total - 1) * probs) + 1
  idx = as.integer(idx)
  idx_unique = unique(idx)
  if (length(idx_unique) < length(idx) && verbose) {
    message("[dbkreg] Some grid points are duplicates; returning unique quantiles.")
  }
  idx = idx_unique
  
  if (length(idx) == 1) {
    idx_sql = as.character(idx)
  } else {
    idx_sql = paste(idx, collapse = ", ")
  }
  
  sql = glue("
    WITH ordered AS (
      SELECT {x} AS x, ROW_NUMBER() OVER (ORDER BY {x}) AS rn
      {from_statement}
    )
    SELECT x AS x0
    FROM ordered
    WHERE rn IN ({idx_sql})
    ORDER BY rn
  ")
  dbGetQuery(conn, sql)$x0
}

#' Build grid source (CTE or temp table)
#' @keywords internal
build_grid_source = function(conn, backend, grid_df, xvars, cte_max = 500L) {
  grid_df = as.data.frame(grid_df)
  grid_df = grid_df[, xvars, drop = FALSE]
  grid_df = unique(grid_df)
  if (!nrow(grid_df)) {
    stop("No valid grid values available.")
  }
  
  if (nrow(grid_df) <= cte_max) {
    values_sql = apply(grid_df, 1, function(r) {
      paste0("(", paste(format_numeric_sql(r), collapse = ", "), ")")
    })
    values_sql = paste(values_sql, collapse = ", ")
    grid_cte = glue("grid({paste(xvars, collapse = ', ')}) AS (VALUES {values_sql})")
    return(list(grid_cte = grid_cte, grid_ref = "grid"))
  }
  
  # Use temp table for larger grids
  tmp_name = temp_table_name(
    sprintf("__dbkreg_%s_grid", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))),
    backend
  )
  values_sql = apply(grid_df, 1, function(r) {
    paste0("(", paste(format_numeric_sql(r), collapse = ", "), ")")
  })
  values_sql = paste(values_sql, collapse = ", ")
  select_sql = glue("SELECT {paste(paste0('v.', xvars, ' AS ', xvars), collapse = ', ')} FROM (VALUES {values_sql}) AS v({paste(xvars, collapse = ', ')})")
  create_temp_table_as(conn, tmp_name, select_sql, backend)
  list(grid_cte = NULL, grid_ref = tmp_name)
}

#' Sample rows from a FROM clause (backend-specific)
#' @keywords internal
sample_from_statement = function(conn, backend, from_statement, n) {
  base_sql = glue("SELECT * {from_statement}")
  if (backend == "sqlserver") {
    sample_sql = glue("SELECT TOP {n} * FROM ({base_sql}) AS base ORDER BY NEWID()")
  } else {
    random_expr = sql_random(backend)
    sample_sql = glue("SELECT * FROM ({base_sql}) AS base ORDER BY {random_expr} LIMIT {n}")
  }
  tmp_name = temp_table_name(
    sprintf("__dbkreg_%s_sample", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))),
    backend
  )
  create_temp_table_as(conn, tmp_name, sample_sql, backend)
  tmp_name
}

#' Kernel weight SQL expression (multi-D product kernel)
#' @keywords internal
kernel_weight_sql_mdim = function(xvars, bandwidth, kernel) {
  parts = vapply(seq_along(xvars), function(i) {
    dx = glue("t.{xvars[i]} - g.{xvars[i]}")
    kernel_weight_sql_1d(dx = dx, h = bandwidth[i], kernel = kernel)
  }, character(1))
  paste0("(", paste(parts, collapse = " * "), ")")
}

#' Kernel weight SQL expression (1D)
#' @keywords internal
kernel_weight_sql_1d = function(dx, h, kernel) {
  h_sql = format_numeric_sql(h)
  u = glue("({dx}) / {h_sql}")
  one_minus_u2 = glue("(1 - POWER({u}, 2))")
  
  switch(
    kernel,
    "uniform" = glue("CASE WHEN ABS({u}) <= 1 THEN 0.5 ELSE 0 END"),
    "epanechnikov" = glue("CASE WHEN ABS({u}) <= 1 THEN 0.75 * {one_minus_u2} ELSE 0 END"),
    "biweight" = glue("CASE WHEN ABS({u}) <= 1 THEN (15.0/16.0) * POWER({one_minus_u2}, 2) ELSE 0 END"),
    "triweight" = glue("CASE WHEN ABS({u}) <= 1 THEN (35.0/32.0) * POWER({one_minus_u2}, 3) ELSE 0 END")
  )
}

#' Format numeric values for SQL
#' @keywords internal
format_numeric_sql = function(x) {
  formatC(x, digits = 15, format = "g")
}
