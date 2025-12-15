#' Run a regression on a database backend.
#'
#' @md
#' @description
#' Leverages the power of databases to run regressions on very large datasets,
#' which may not fit into R's memory. Various acceleration strategies allow for
#' highly efficient computation, while robust standard errors are computed from
#' sufficient statistics.
#'
#' @param fml A \code{\link[stats]{formula}} representing the relation to be
#' estimated. Fixed effects should be included after a pipe, e.g
#' `fml = y ~ x1 + x2 | fe1 + f2`. Currently, only simple additive terms
#' are supported (i.e., no interaction terms, transformations or literals).
#' @param conn Database connection, e.g. created with
#' \code{\link[DBI]{dbConnect}}. Can be either persistent (disk-backed) or
#' ephemeral (in-memory). If no connection is provided, then an ephemeral
#' \code{\link[duckdb]{duckdb}} connection will be created automatically and
#' closed before the function exits. Note that a persistent (disk-backed)
#' database connection is required for larger-than-RAM datasets in order to take
#' advantage of out-of-core functionality like streaming (where supported).
#' @param table,data,path Mutually exclusive arguments for specifying the data
#' table (object) to be queried. In order of precedence:
#' - `table`: Character string giving the name of the data table in an
#' existing (open) database connection.
#' - `data`: R dataframe that can be copied over to `conn` as a temporary
#' table for querying via the DuckDB query engine. Ignored if `table` is
#' provided.
#' - `path`: Character string giving a path to the data file(s) on disk, which
#' will be read into `conn`. Internally, this string is passed to the `FROM`
#' query statement, so could (should) include file globbing for
#' Hive-partitioned datasets, e.g. `"mydata/**/.*parquet"`. For more precision,
#' however, it is recommended to pass the desired database reader function as
#' part of this string, e.g. `"read_parquet('mydata/**/*.parquet')"` for DuckDB;
#' note the use of single quotes.
#' Ignored if either `table` or `data` is provided.
#' @param vcov Character string denoting the desired type of variance-
#' covariance correction / standard errors. At present, only "iid" (default) or
#' "hc1" (heteroskedasticity-consistent) are supported.
#' @param strategy Character string indicating the preferred acceleration
#'   strategy. The default `"auto"` will pick an optimal strategy based on
#'   internal heuristics. Users can also override with one of the following
#'   explicit strategies: `"compress"`, `"demean"` (alias: `"within"`), or
#'   `"moments"`. See the Acceleration Strategies section below for details.
#' @param compress_ratio,compress_nmax Numeric(s). Parameters that help to
#'   determine the acceleration `strategy` under the default `"auto"` option.
#'
#'   - `compress_ratio` defines the compression ratio threshold, i.e. compressed
#'     data size vs. original data size. An estimated compression ratio larger
#'     than this threshold indicates poor compression relative to the desired
#'     level.
#'   - `compress_nmax` defines the maximum allowable size (in rows) of the
#'     compressed dataset that can be serialized into R. Pays heed to the idea
#'     that big data serialization can be costly (esp. for remote databases),
#'     even if we have achieved good compression on top of the original dataset.
#'
#' If both conditions are met, i.e. (1) estimated compression ratio <
#' `compress_ratio` and (2) estimated compressed data size < `compress_nmax`,
#' then the `"compress"` strategy is used. Otherwise, either the `"demean"` or
#' `"moments"` strategy will be used, depending on the number of fixed effects.
#' @param query_only Logical indicating whether only the underlying compression
#'   SQL query should be returned (i.e., no computation will be performed).
#'   Default is `FALSE`.
#' @param data_only Logical indicating whether only the compressed dataset
#'   should be returned (i.e., no regression is run). Default is `FALSE`.
#' @param drop_missings Logical indicating whether incomplete cases (i.e., rows
#'   where any of the dependent, independent or fixed effects variables are
#'   missing) should be dropped. The default is `TRUE`, according with standard
#'   regression software. It is *strongly* recommended not to change this value
#'   unless you are absolutely sure that your data have no missings and you wish
#'   to skip some internal checks. (Even then, it probably isn't worth it.)
#' @param verbose Logical. Print progress messages to the console? Defaults to
#'   `TRUE`.
#'
#' @return A list of class "dbreg" containing various slots, including a table
#' of coefficients (which the associated print method will display).
#'
#' @section Acceleration strategies:
#'
#' `dbreg` offers three primary acceleration (shortcut) strategies for
#' estimating regression results from simplied data representations:
#'
#' 1. `"compress"`: compresses the data via a `GROUP BY` operation (using the explanatory variables and fixed effects as groups), before running weighted least squares on this much smaller dataset:
#'    \deqn{\hat{\beta} = (X_c' W X_c)^{-1} X_c' W Y_c}
#'    where \eqn{W = \text{diag}(n_g)} are the group frequencies. This procedure follows Wang et al. (2021).
#' 2. `"moments"`: computes sufficient statistics (\eqn{X'X, X'y}) directly via SQL aggregation, returning a single-row result. This solves the standard OLS normal equations \eqn{\hat{\beta} = (X'X)^{-1}X'y}. Limited to cases without fixed effects.
#' 3. `"demean"` (alias `"within"`): subtracts group-level means from both Y and X before computing sufficient statistics (per the `"moments"` strategy). For two-way fixed effects:
#'    \deqn{\ddot{Y}_{it} = \beta \ddot{X}_{it} + \varepsilon_{it}}
#'    where \eqn{\ddot{X} = X - \bar{X}_i - \bar{X}_t + \bar{X}}. This within estimator gives identical coefficients to fixed effects regression. Permits at most two fixed effects.
#'
#' The relative efficiency of each of these strategies depends on the size and
#' structure of the data, as well the number of unique regressors and
#' fixed effects. While the compression approach can yield remarkable
#' performance gains for "standard" cases, it is less efficient for a true panel
#' (repeated cross-sections over time), where N >> T. In such cases, it is more
#' efficient to use a demeaning (within) transformation that subtracts group means
#' first. (Reason: unit and time fixed effects are typically high dimensional,
#' but covariate averages are not.)
#'
#' If the user does not specify an explicit acceleration strategy, then
#' `dbreg` will invoke an `"auto"` heuristic behind the scenes. This requires
#' some additional overhead, but in most cases should be negligible next to the
#' overall time savings. The heuristic is as follows:
#'
#' - IF no fixed effects AND (any continuous regressor OR poor compression ratio OR too big compressed data) THEN `"moments"`.
#' - ELSE IF 1-2 fixed effects AND (poor compression ratio OR too big compressed data) THEN `"demean"`.
#' - ELSE THEN `"compress"`.
#'
#' @references
#' Arkhangelsky, D. & Imbens, G. (2024)
#' \cite{Fixed Effects and the Generalized Mundlak Estimator}.
#' The Review of Economic Studies, 91(5), pp. 2545–2571.
#' Available: https://doi.org/10.1093/restud/rdad089
#'
#' Wong, J., Forsell, E., Lewis, R., Mao, T., & Wardrop, M. (2021).
#' \cite{You Only Compress Once: Optimal Data Compression for Estimating Linear Models.}
#' arXiv preprint arXiv:2102.11297.
#' Available: https://doi.org/10.48550/arXiv.2102.11297
#'
#' @seealso \code{\link[DBI]{dbConnect}} for creating database connections,
#' \code{\link[duckdb]{duckdb}} for DuckDB-specific connections
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetInfo dbGetQuery
#' @importFrom duckdb duckdb duckdb_register
#' @importFrom Formula Formula
#' @importFrom Matrix chol2inv crossprod Diagonal sparse.model.matrix
#' @importFrom stats formula reformulate pt
#' @importFrom glue glue glue_sql
#'
#' @examples
#'
#' # A not very compelling example using a small in-memory dataset:
#' (mod = dbreg(Temp ~ Wind | Month, data = airquality))
#'
#' # Same result as lm
#' summary(lm(Temp ~ Wind + factor(Month), data = airquality))
#'
#' # Aside: dbreg's default print method hides the "nuisance" coefficients
#' # like the intercept and fixed effect(s). But we can grab them if we want.
#' print(mod, fes = TRUE)
#'
#' # Note: for a more compelling and appropriate use-case, i.e. regression on a
#' # big (~180 million row) dataset of Hive-partioned parquet files, see the
#' # package website:
#' # https://github.com/grantmcdermott/dbreg?tab=readme-ov-file#quickstart
#' @export
dbreg = function(
  fml,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  vcov = c("iid", "hc1"),
  strategy = c("auto", "compress", "moments", "demean", "within"),
  compress_ratio = 0.001,
  compress_nmax = 1e6,
  query_only = FALSE,
  data_only = FALSE,
  drop_missings = TRUE,
  verbose = TRUE
) {
  vcov = tolower(vcov)
  vcov = match.arg(vcov)
  strategy = match.arg(strategy)
  if (strategy == "within") strategy = "demean"  # alias

  # Process and validate inputs
  inputs = process_dbreg_inputs(
    fml = fml,
    conn = conn,
    table = table,
    data = data,
    path = path,
    vcov = vcov,
    strategy = strategy,
    query_only = query_only,
    data_only = data_only,
    compress_ratio = compress_ratio,
    compress_nmax = compress_nmax,
    drop_missings = drop_missings,
    verbose = verbose
  )

  # Choose strategy
  chosen_strategy = choose_strategy(inputs)

  # Execute chosen strategy
  result = switch(
    chosen_strategy,
    # sufficient statistics with no fixed effects
    "moments" = execute_moments_strategy(inputs),
    # one or two-way fixed effects (double demeaning / within estimator)
    "demean" = execute_demean_strategy(inputs),
    # group by regressors (+ fixed effects) -> frequency-weighted rows -> WLS
    # best when regressors are discrete and FE groups have many rows per unique value
    "compress" = execute_compress_strategy(inputs),
    stop("Unknown strategy: ", chosen_strategy)
  )
  # Finalize result
  finalize_dbreg_result(result, inputs, chosen_strategy)
}

#' Process and validate dbreg inputs
#' @keywords internal
process_dbreg_inputs = function(
  fml,
  conn,
  table,
  data,
  path,
  vcov,
  strategy,
  query_only,
  data_only,
  compress_ratio,
  compress_nmax,
  drop_missings,
  verbose
) {
  vcov_type_req = vcov
  own_conn = FALSE
  if (is.null(conn)) {
    conn = dbConnect(duckdb(), shutdown = TRUE)
    own_conn = TRUE
    #  on.exit(try(dbDisconnect(conn), silent = TRUE), add = TRUE)
  }

  # FROM clause
  if (!is.null(table)) {
    if (is.character(table)) {
      # Original behavior: table name
      from_statement = glue("FROM {table}")
    } else if (inherits(table, "tbl_lazy")) {
      # lazy table: render SQL and try to extract connection
      rendered_sql = tryCatch(dbplyr::sql_render(table), error = function(e) {
        NULL
      })
      if (is.null(rendered_sql)) {
        stop("Failed to render SQL for provided tbl_lazy.")
      }
      from_statement = paste0("FROM (", rendered_sql, ") AS lazy_subquery")
      if (is.null(conn)) {
        # try to extract DBI connection from the tbl_lazy (tbl_dbi stores it at src$con)
        if (!is.null(table$src) && !is.null(table$src$con)) {
          conn = table$src$con
        } else {
          stop(
            "`conn` is NULL and could not be extracted from the provided tbl_lazy. Provide `conn` explicitly."
          )
        }
      }
    } else {
      stop("`table` must be character or tbl_lazy object.")
    }
  } else if (!is.null(data)) {
    if (!inherits(data, "data.frame")) {
      stop("`data` must be data.frame.")
    }
    duckdb_register(conn, "tmp_table_dbreg", data)
    from_statement = "FROM tmp_table_dbreg"
  } else if (!is.null(path)) {
    if (!is.character(path)) {
      stop("`path` must be character.")
    }
    if (!(grepl("^read|^scan", path) && grepl("'", path))) {
      path = gsub('"', "'", path)
      from_statement = glue("FROM '{path}'")
    } else {
      from_statement = glue("FROM {path}")
    }
  } else {
    stop("Provide one of `table`, `data`, or `path`.")
  }

  # Parse formula
  fml = Formula(fml)
  yvar = all.vars(formula(fml, lhs = 1, rhs = 0))
  if (length(yvar) != 1) {
    stop("Exactly one outcome variable required.")
  }

  xvars = all.vars(formula(fml, lhs = 0, rhs = 1))
  fes = if (length(fml)[2] > 1) {
    all.vars(formula(fml, lhs = 0, rhs = 2))
  } else {
    NULL
  }
  if (!length(xvars)) {
    stop("No regressors on RHS.")
  }

  # Heuristic for continuous regressors (only if data passed)
  is_continuous = function(v) {
    if (is.null(data)) {
      return(NA)
    }
    xv = data[[v]]
    if (is.integer(xv)) {
      return(FALSE)
    }
    if (is.numeric(xv)) {
      return(length(unique(xv)) > min(50, 0.2 * length(xv)))
    }
    TRUE
  }
  any_continuous = if (!is.null(data)) {
    any(vapply(xvars, is_continuous, logical(1)))
  } else {
    FALSE
  }

    # Filter missing cases
  if (isTRUE(drop_missings)) {
    from_statement = glue("
    {from_statement}
    WHERE {yvar} IS NOT NULL
    AND {paste(xvars, collapse = ' IS NOT NULL AND ')} IS NOT NULL
    "
    )
    if (!is.null(fes)) {
      from_statement = glue("
      {from_statement}
      AND {paste(fes, collapse = ' IS NOT NULL AND ')} IS NOT NULL
      ")
    }
  }

  list(
    fml = fml,
    yvar = yvar,
    xvars = xvars,
    fes = fes,
    conn = conn,
    from_statement = from_statement,
    data = data,
    vcov_type_req = vcov_type_req,
    strategy = strategy,
    query_only = query_only,
    data_only = data_only,
    compress_ratio = compress_ratio,
    compress_nmax = compress_nmax,
    verbose = verbose,
    any_continuous = any_continuous,
    own_conn = own_conn
  )
}

#' Check if the database backend supports COUNT_BIG
#'
#' This function checks whether the provided database connection is to a backend
#' that supports the `COUNT_BIG` function, such as SQL Server or Azure SQL.
#'
#' @param conn A DBI database connection object.
#'
#' @return Logical value: `TRUE` if the backend supports `COUNT_BIG`, `FALSE` otherwise.
#' @examples
#' \dontrun{
#'   con = DBI::dbConnect(odbc::odbc(), ...)
#'   backend_supports_count_big(con)
#' }
#' @export
backend_supports_count_big = function(conn) {
  info = try(dbGetInfo(conn), silent = TRUE)
  if (inherits(info, "try-error")) {
    return(FALSE)
  }
  dbms = tolower(paste(info$dbms.name, collapse = " "))
  grepl("sql server|azure sql|microsoft sql server", dbms)
}

# detect SQL backend
detect_backend = function(conn) {
  info = try(dbGetInfo(conn), silent = TRUE)
  if (inherits(info, "try-error")) {
    return(list(name = "unknown", supports_count_big = FALSE))
  }
  dbms = tolower(paste(info$dbms.name, collapse = " "))
  list(
    name = if (grepl("duckdb", dbms)) {
      "duckdb"
    } else if (grepl("sql server|azure sql|microsoft sql server", dbms)) {
      "sqlserver"
    } else {
      "other"
    },
    supports_count_big = grepl(
      "sql server|azure sql|microsoft sql server",
      dbms
    )
  )
}

# sql_count: returns an expression fragment for use inside SELECT when possible.
sql_count = function(conn, alias, expr = "*", distinct = FALSE) {
  bd = detect_backend(conn)
  if (distinct) {
    glue(
      "{if (bd$supports_count_big) paste0('COUNT_BIG(DISTINCT ', expr, ')') else paste0('CAST(COUNT(DISTINCT ', expr, ') AS BIGINT)')} AS {alias}"
    )
  } else {
    if (bd$supports_count_big) {
      glue("COUNT_BIG({expr}) AS {alias}")
    } else {
      glue("CAST(COUNT({expr}) AS BIGINT) AS {alias}")
    }
  }
}

#' Choose regression strategy based on inputs and auto logic
#' @keywords internal
choose_strategy = function(inputs) {
  # Extract values
  strategy = inputs$strategy
  fes = inputs$fes
  verbose = inputs$verbose
  any_continuous = inputs$any_continuous
  compress_ratio = inputs$compress_ratio
  compress_nmax = inputs$compress_nmax
  conn = inputs$conn
  from_statement = inputs$from_statement
  xvars = inputs$xvars

  # Compression ratio estimator
  estimate_compression = function(inputs) {
    conn = inputs$conn
    verbose = inputs$verbose
    xvars = inputs$xvars
    fes = inputs$fes
    from_statement = inputs$from_statement

    if (verbose) {
      message("[dbreg] Estimating compression ratio and data size...")
    }
    key_cols = c(xvars, fes)
    if (!length(key_cols)) {
      return(1)
    }

    # Total rows (safe: COUNT(*) is supported pretty much everywhere)
    total_sql = glue(
      "SELECT CAST(COUNT(*) AS BIGINT) AS n FROM (SELECT * {from_statement}) t"
    )
    total_n = dbGetQuery(conn, total_sql)$n

    # Helper to count distinct tuples (works for single or multi-column)
    count_distinct_tuples = function(cols) {
      cols_expr = paste(cols, collapse = ", ")
      # Use subquery counting distinct tuples (portable and works in DuckDB/SQL Server/etc)
      sql = glue(
        "SELECT CAST(COUNT(*) AS BIGINT) AS g FROM (SELECT DISTINCT {cols_expr} {from_statement}) t"
      )
      dbGetQuery(conn, sql)$g
    }

    if (length(fes)) {
      # count unique FE groups (may be single or multi-column)
      n_groups_fe = tryCatch(count_distinct_tuples(fes), error = function(e) {
        NA_integer_
      })
    } else {
      n_groups_fe = NA_integer_
    }

    # count unique keys over regressors + FEs (may be multi-column)
    n_groups_total = tryCatch(
      count_distinct_tuples(key_cols),
      error = function(e) NA_integer_
    )

    if (verbose && length(fes) && !is.na(n_groups_fe)) {
      message(
        "[dbreg] Data has ",
        format(total_n, big.mark = ","),
        " rows and ",
        format(n_groups_fe, big.mark = ","),
        " unique FE groups."
      )
    }

    comp_rat = n_groups_total / max(total_n, 1)
    attr(comp_rat, "comp_size") = n_groups_total

    return(comp_rat)
  }

  chosen_strategy = strategy
  est_cr = NA_real_

  # Auto logic
  if (strategy == "auto") {
    est_cr = tryCatch(estimate_compression(inputs), error = function(e) {
      NA_real_
    })
    comp_size = attr(est_cr, "comp_size")
    if (length(fes) == 0) {
      fail_compress_ratio = !is.na(est_cr) && est_cr > compress_ratio
      fail_compress_nmax = !is.na(est_cr) && comp_size > compress_nmax
      if (any_continuous || (fail_compress_ratio || fail_compress_nmax)) {
        chosen_strategy = "moments"
      } else {
        chosen_strategy = "compress"
      }
    } else if (length(fes) %in% c(1, 2)) {
      fail_compress_ratio = !is.na(est_cr) && est_cr > max(0.6, compress_ratio)
      fail_compress_nmax = !is.na(est_cr) && comp_size > compress_nmax
      if (fail_compress_ratio || fail_compress_nmax) {
        chosen_strategy = "demean"
        if (verbose) {
          if (fail_compress_ratio) {
            reason = paste0(
              "compression ratio (",
              sprintf("%.2f", est_cr),
              ") > threshold (",
              max(0.6, compress_ratio),
              ")."
            )
          } else {
            reason = paste0(
              "compressed data size (",
              prettyNum(comp_size, big.mark = ","),
              " rows) > threshold (",
              prettyNum(compress_nmax, big.mark = ","),
              " rows)."
            )
          }
          message("[dbreg] Auto: ", reason)
          message("[dbreg] Auto: selecting demean")
        }
      } else {
        chosen_strategy = "compress"
      }
    } else {
      chosen_strategy = "compress"
      if (!is.na(est_cr) && est_cr > 0.8 && verbose) {
        message(sprintf(
          "[dbreg] Auto: high compression ratio (%.4f). Group compression preferred for this FE structure.",
          est_cr
        ))
      }
    }
    if (verbose && (strategy != "auto")) {
      message(
        "Compression ratio: ",
        ifelse(is.na(est_cr), "unknown", sprintf("%.2f", est_cr))
      )
    }
  } else {
    chosen_strategy = strategy
  }

  if (verbose) {
    message("[dbreg] Using strategy: ", chosen_strategy)
  }

  # Guard unsupported combos
  if (chosen_strategy == "moments" && length(fes) > 0) {
    if (verbose) {
      message(
        "[dbreg] FE present; moments (no-FE) not applicable. Using compress."
      )
    }
    chosen_strategy = "compress"
  }
  if (chosen_strategy == "demean" && !(length(fes) %in% c(1, 2))) {
    if (verbose) {
      message("[dbreg] demean requires one or two FEs. Using compress.")
    }
    chosen_strategy = "compress"
  }

  # Store compression ratio estimate for later use
  inputs$compression_ratio_est = est_cr

  chosen_strategy
}

#' Execute moments strategy (no fixed effects)
#' @keywords internal
execute_moments_strategy = function(inputs) {
  pair_exprs = c(
    "COUNT(*) AS n_total",
    glue("SUM({inputs$yvar}) AS sum_y"),
    glue("SUM({inputs$yvar}*{inputs$yvar}) AS sum_y_sq")
  )
  for (x in inputs$xvars) {
    pair_exprs = c(
      pair_exprs,
      glue("SUM({x}) AS sum_{x}"),
      glue("SUM({x}*{inputs$yvar}) AS sum_{x}_y"),
      glue("SUM({x}*{x}) AS sum_{x}_{x}")
    )
  }
  xpairs = gen_xvar_pairs(inputs$xvars)
  for (pair in xpairs) {
    xi = pair[1]
    xj = pair[2]
    pair_exprs = c(pair_exprs, glue("SUM({xi}*{xj}) AS sum_{xi}_{xj}"))
  }
  moments_sql = paste0(
    "SELECT\n  ",
    paste(pair_exprs, collapse = ",\n  "),
    "\n",
    inputs$from_statement
  )

  if (inputs$query_only) {
    return(moments_sql)
  }
  if (inputs$verbose) {
    message("[dbreg] Executing moments SQL\n")
  }
  moments_df = dbGetQuery(inputs$conn, moments_sql)
  if (inputs$data_only) {
    return(moments_df)
  }
  n_total = moments_df$n_total

  vars_all = c("(Intercept)", inputs$xvars)
  p = length(vars_all)
  XtX = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  Xty = matrix(0, p, 1, dimnames = list(vars_all, ""))

  XtX["(Intercept)", "(Intercept)"] = n_total
  Xty["(Intercept)", ] = moments_df$sum_y
  for (x in inputs$xvars) {
    sx = moments_df[[paste0("sum_", x)]]
    sxx = moments_df[[paste0("sum_", x, "_", x)]]
    sxy = moments_df[[paste0("sum_", x, "_y")]]
    XtX["(Intercept)", x] = XtX[x, "(Intercept)"] = sx
    XtX[x, x] = sxx
    Xty[x, ] = sxy
  }
  xpairs = gen_xvar_pairs(inputs$xvars)
  for (pair in xpairs) {
    xi = pair[1]
    xj = pair[2]
    val = moments_df[[paste0("sum_", xi, "_", xj)]]
    XtX[xi, xj] = XtX[xj, xi] = val
  }

  solve_result = solve_with_fallback(XtX, Xty)
  betahat = solve_result$betahat
  XtX_inv = solve_result$XtX_inv
  rownames(betahat) = vars_all

  rss = as.numeric(
    moments_df$sum_y_sq -
      2 * t(betahat) %*% Xty +
      t(betahat) %*% XtX %*% betahat
  )
  df_res = max(n_total - p, 1)
  # Calculate TSS for R2
  sum_y = moments_df$sum_y
  sum_y_sq = moments_df$sum_y_sq
  tss = sum_y_sq - (sum_y^2 / n_total)
  
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "moments",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = inputs$fml,
    yvar = inputs$yvar,
    xvars = inputs$xvars,
    fes = NULL,
    query_string = moments_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "moments",
    compression_ratio_est = inputs$compression_ratio_est,
    df_residual = df_res
  )
}

#' Execute demean strategy (1-2 fixed effects)
#' 
#' Double demeaning / within estimator. Gives identical coefficients to 
#' fixed effects regression.
#' 
#' @keywords internal
execute_demean_strategy = function(inputs) {
  if (length(inputs$fes) == 1) {
    # Single FE: simple within-group demeaning
    fe1 = inputs$fes[1]
    all_vars = c(inputs$yvar, inputs$xvars)

    means_cols = paste(
      sprintf("AVG(%s) AS %s_mean", all_vars, all_vars),
      collapse = ", "
    )
    tilde_exprs = paste(
      sprintf("(b.%s - gm.%s_mean) AS %s_tilde", all_vars, all_vars, all_vars),
      collapse = ",\n       "
    )

    moment_terms = c(
      sql_count(inputs$conn, "n_total"),
      sql_count(inputs$conn, "n_fe1", fe1, distinct = TRUE),
      "1 AS n_fe2",
      sprintf(
        "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_y_sq",
        inputs$yvar,
        inputs$yvar
      )
    )
    for (x in inputs$xvars) {
      moment_terms = c(
        moment_terms,
        sprintf(
          "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_%s_%s",
          x,
          inputs$yvar,
          x,
          inputs$yvar
        ),
        sprintf(
          "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_%s_%s",
          x,
          x,
          x,
          x
        )
      )
    }
    xpairs = gen_xvar_pairs(inputs$xvars)
    for (pair in xpairs) {
      xi = pair[1]
      xj = pair[2]
      moment_terms = c(
        moment_terms,
        sprintf(
          "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_%s_%s",
          xi,
          xj,
          xi,
          xj
        )
      )
    }

    demean_sql = paste0(
      "WITH base AS (
      SELECT * ",
      inputs$from_statement,
      "
      ),
      group_means AS (
      SELECT ",
      fe1,
      ", ",
      means_cols,
      " FROM base GROUP BY ",
      fe1,
      "
      ),
      demeaned AS (
      SELECT
          b.",
      fe1,
      ",
          ",
      tilde_exprs,
      "
      FROM base b
      JOIN group_means gm ON b.",
      fe1,
      " = gm.",
      fe1,
      "
      ),
      moments AS (
      SELECT
          ",
      paste(moment_terms, collapse = ",\n    "),
      "
      FROM demeaned
      )
      SELECT * FROM moments"
    )
  } else {
    # Two FE: double demeaning
    fe1 = inputs$fes[1]
    fe2 = inputs$fes[2]
    all_vars = c(inputs$yvar, inputs$xvars)

    unit_means_cols = paste(
      sprintf("AVG(%s) AS %s_u", all_vars, all_vars),
      collapse = ", "
    )
    time_means_cols = paste(
      sprintf("AVG(%s) AS %s_t", all_vars, all_vars),
      collapse = ", "
    )
    overall_cols = paste(
      sprintf("AVG(%s) AS %s_o", all_vars, all_vars),
      collapse = ", "
    )
    tilde_exprs = paste(
      sprintf(
        "(b.%s - um.%s_u - tm.%s_t + o.%s_o) AS %s_tilde",
        all_vars,
        all_vars,
        all_vars,
        all_vars,
        all_vars
      ),
      collapse = ",\n       "
    )

    moment_terms = c(
      sql_count(inputs$conn, "n_total"),
      sql_count(inputs$conn, "n_fe1", fe1, distinct = TRUE),
      sql_count(inputs$conn, "n_fe2", fe2, distinct = TRUE),
      sprintf(
        "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_y_sq",
        inputs$yvar,
        inputs$yvar
      )
    )
    for (x in inputs$xvars) {
      moment_terms = c(
        moment_terms,
        sprintf(
          "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_%s_%s",
          x,
          inputs$yvar,
          x,
          inputs$yvar
        ),
        sprintf(
          "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_%s_%s",
          x,
          x,
          x,
          x
        )
      )
    }
    xpairs = gen_xvar_pairs(inputs$xvars)
    for (pair in xpairs) {
      xi = pair[1]
      xj = pair[2]
      moment_terms = c(
        moment_terms,
        sprintf(
          "SUM(CAST(%s_tilde AS FLOAT) * CAST(%s_tilde AS FLOAT)) AS sum_%s_%s",
          xi,
          xj,
          xi,
          xj
        )
      )
    }

    demean_sql = paste0(
      "WITH base AS (
      SELECT * ",
      inputs$from_statement,
      "
      ),
      unit_means AS (
      SELECT ",
      fe1,
      ", ",
      unit_means_cols,
      " FROM base GROUP BY ",
      fe1,
      "
      ),
      time_means AS (
      SELECT ",
      fe2,
      ", ",
      time_means_cols,
      " FROM base GROUP BY ",
      fe2,
      "
      ),
      overall AS (
      SELECT ",
      overall_cols,
      " FROM base
      ),
      demeaned AS (
      SELECT
          b.",
      fe1,
      ",
          b.",
      fe2,
      ",
          ",
      tilde_exprs,
      "
      FROM base b
      JOIN unit_means um ON b.",
      fe1,
      " = um.",
      fe1,
      "
      JOIN time_means tm ON b.",
      fe2,
      " = tm.",
      fe2,
      "
      CROSS JOIN overall o
      ),
      moments AS (
      SELECT
          ",
      paste(moment_terms, collapse = ",\n    "),
      "
      FROM demeaned
      )
      SELECT * FROM moments"
    )
  }

  # Athena FLOAT gotcha
  # https://github.com/DyfanJones/noctua/issues/228
  if (inherits(inputs$conn, "AthenaConnection")) {
    demean_sql = gsub("FLOAT", "REAL", demean_sql, fixed = TRUE)
  }

  if (inputs$query_only) {
    return(demean_sql)
  }

  # Execute SQL and build matrices
  if (inputs$verbose) {
    message("[dbreg] Executing demean SQL\n")
  }
  demean_df = dbGetQuery(inputs$conn, demean_sql)
  if (inputs$data_only) {
    return(demean_df)
  }
  n_total = demean_df$n_total
  n_fe1 = demean_df$n_fe1
  n_fe2 = demean_df$n_fe2

  vars_all = inputs$xvars # No intercept for FE models
  p = length(vars_all)
  XtX = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  Xty = matrix(0, p, 1, dimnames = list(vars_all, ""))

  for (x in inputs$xvars) {
    XtX[x, x] = demean_df[[sprintf("sum_%s_%s", x, x)]]
    Xty[x, ] = demean_df[[sprintf("sum_%s_%s", x, inputs$yvar)]]
  }
  if (length(inputs$xvars) > 1) {
    for (i in seq_along(inputs$xvars)) {
      if (i == 1) {
        next
      }
      for (j in seq_len(i - 1)) {
        xi = inputs$xvars[i]
        xj = inputs$xvars[j]
        XtX[xi, xj] = XtX[xj, xi] = demean_df[[sprintf("sum_%s_%s", xi, xj)]]
      }
    }
  }

  solve_result = solve_with_fallback(XtX, Xty)
  betahat = solve_result$betahat
  XtX_inv = solve_result$XtX_inv
  rownames(betahat) = vars_all

  rss = as.numeric(
    demean_df$sum_y_sq -
      2 * t(betahat) %*% Xty +
      t(betahat) %*% XtX %*% betahat
  )
  df_fe = n_fe1 + n_fe2 - 1
  df_res = max(n_total - p - df_fe, 1)
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "demean",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = demean_df$sum_y_sq

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = inputs$fml,
    yvar = inputs$yvar,
    xvars = inputs$xvars,
    fes = inputs$fes,
    query_string = demean_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "demean",
    compression_ratio_est = inputs$compression_ratio_est,
    df_residual = df_res,
    n_fe1 = n_fe1,
    n_fe2 = n_fe2
  )
}

#' Execute compress strategy (groupby compression)
#' @keywords internal
execute_compress_strategy = function(inputs) {
  from_statement = inputs$from_statement
  # catch for sampled (limited) queries
  if (grepl("LIMIT\\s+\\d+\\s*$", from_statement, ignore.case = TRUE)) {
    from_statement = glue("FROM (SELECT * {from_statement})")
  }

  group_cols = c(inputs$xvars, inputs$fes)
  group_cols_sql = paste(group_cols, collapse = ", ")
  query_string = paste0(
    "WITH cte AS (
    SELECT
        ",
    group_cols_sql,
    ",
        COUNT(*) AS n,
        SUM(",
    inputs$yvar,
    ") AS sum_Y,
        SUM(POWER(",
    inputs$yvar,
    ", 2)) AS sum_Y_sq
    ",
    from_statement,
    "
    GROUP BY ",
    group_cols_sql,
    "
    )
    SELECT
    *,
    sum_Y / n AS mean_Y,
    sqrt(n) AS wts
    FROM cte"
  )

  if (inputs$query_only) {
    return(query_string)
  }
  if (inputs$verbose) {
    message("[dbreg] Executing compress strategy SQL\n")
  }
  compressed_dat = dbGetQuery(inputs$conn, query_string)
  nobs_orig = sum(compressed_dat$n)
  nobs_comp = nrow(compressed_dat)
  compression_ratio = nobs_comp / max(nobs_orig, 1)

  if (inputs$verbose && compression_ratio > 0.8) {
    warning(sprintf(
      "[dbreg] compression ineffective (%.1f%% of original rows).",
      100 * compression_ratio
    ))
  }

  if (length(inputs$fes)) {
    for (f in inputs$fes) {
      compressed_dat[[f]] = factor(compressed_dat[[f]])
    }
  }
  if (inputs$data_only) {
    return(compressed_dat)
  }

  X = sparse.model.matrix(
    reformulate(c(inputs$xvars, inputs$fes)),
    compressed_dat
  )
  if (ncol(X) == 0) {
    stop("Design matrix has zero columns.")
  }
  Y = compressed_dat[, "mean_Y"]
  wts = compressed_dat[["wts"]]
  Xw = X * wts
  Yw = Y * wts
  XtX = crossprod(Xw)
  XtY = crossprod(Xw, Yw)

  solve_result = solve_with_fallback(XtX, XtY)
  betahat = solve_result$betahat
  XtX_inv = solve_result$XtX_inv
  if (is.null(dim(betahat))) {
    betahat = matrix(betahat, ncol = 1)
  }
  rownames(betahat) = colnames(X)
  yhat = as.numeric(X %*% betahat)

  n_vec = compressed_dat$n
  sum_Y = compressed_dat$sum_Y
  sum_Y_sq = compressed_dat$sum_Y_sq
  rss_g = sum_Y_sq - 2 * yhat * sum_Y + n_vec * (yhat^2)
  rss_total = sum(rss_g)
  df_res = max(nobs_orig - ncol(X), 1)

  # Calculate TSS for R2
  sum_Y_total = sum(compressed_dat$sum_Y)
  sum_Y_sq_total = sum(compressed_dat$sum_Y_sq)
  tss = sum_Y_sq_total - (sum_Y_total^2 / nobs_orig)
  
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "compress",
    XtX_inv = XtX_inv,
    rss = rss_total,
    df_res = df_res,
    nobs_orig = nobs_orig,
    X = X,
    rss_g = rss_g
  )
  attr(vcov_mat, "rss") = rss_total
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, max(nobs_orig - ncol(X), 1))

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = inputs$fml,
    yvar = inputs$yvar,
    xvars = inputs$xvars,
    fes = inputs$fes,
    query_string = query_string,
    nobs = nobs_comp,
    nobs_orig = nobs_orig,
    strategy = "compress",
    compression_ratio = compression_ratio,
    compression_ratio_est = inputs$compression_ratio_est,
    df_residual = max(nobs_orig - ncol(X), 1)
  )
}

#' Solve linear system using Cholesky but with QR fallback
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

#' Compute variance-covariance matrix
#' @keywords internal
compute_vcov = function(
  vcov_type = "iid",
  strategy = "compress",
  XtX_inv,
  rss,
  df_res,
  nobs_orig,
  X = NULL,
  rss_g = NULL
) {
  if (vcov_type == "hc1") {
    if (strategy == "compress") {
      # Compress strategy: HC1 with grouped residuals
      meat = crossprod(X, Diagonal(x = as.numeric(rss_g)) %*% X)
      scale_hc1 = nobs_orig / df_res
      vcov_mat = scale_hc1 * (XtX_inv %*% meat %*% XtX_inv)
    } else {
      # Moments/Demean strategy: simple HC1 scaling
      sigma2 = rss / df_res
      vcov_mat = sigma2 * XtX_inv * (nobs_orig / df_res)
    }
    attr(vcov_mat, "type") = "hc1"
  } else {
    # IID case (same for all strategies)
    sigma2 = rss / df_res
    vcov_mat = sigma2 * XtX_inv
    attr(vcov_mat, "type") = "iid"
  }
  dimnames(vcov_mat) = dimnames(XtX_inv)
  vcov_mat
}

#' Generate unique pairs of variables (preserves original nested loop order)
#' @keywords internal
gen_xvar_pairs = function(xvars) {
  pairs = list()
  if (length(xvars) > 1) {
    for (i in seq_along(xvars)) {
      if (i == 1) {
        next
      }
      for (j in seq_len(i - 1)) {
        pairs = c(pairs, list(c(xvars[i], xvars[j])))
      }
    }
  }
  pairs
}

#' Generate coefficient table from estimates and vcov matrix
#' @keywords internal
gen_coeftable = function(betahat, vcov_mat, df_residual) {
  coefs = as.numeric(betahat)
  names(coefs) = rownames(betahat)
  ses = sqrt(Matrix::diag(vcov_mat))
  tstats = coefs / ses
  pvals = 2 * pt(-abs(tstats), df_residual)
  cbind(estimate = coefs, std.error = ses, statistic = tstats, p.values = pvals)
}

#' Finalize dbreg result object
#' @keywords internal
finalize_dbreg_result = function(result, inputs, chosen_strategy) {
  if (inputs$query_only) {
    cat(result)
    return(invisible(result))
  }
  if (inputs$data_only) {
    return(result)
  }
  result$strategy = chosen_strategy
  class(result) = c("dbreg", class(result))
  result
}
