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
#' covariance correction / standard errors. At present, only `"iid"` (default)
#' or `"hc1"` (heteroskedasticity-consistent) are supported. Note that the
#' latter requires a second pass over the data unless `strategy = "compress"` to
#' construct the residuals.
#' @param strategy Character string indicating the preferred acceleration
#'   strategy. The default `"auto"` will pick an optimal strategy based on
#'   internal heuristics. Users can also override with one of the following
#'   explicit strategies: `"compress"`, `"demean"` (alias: `"within"`),
#'   `"mundlak"`, or `"moments"`. See the Acceleration Strategies section below
#'   for details.
#' @param compress_ratio,compress_nmax Numeric(s). Parameters that help to
#'   determine the acceleration `strategy` under the default `"auto"` option.
#'
#'   - `compress_ratio` defines the compression ratio threshold, i.e. numeric
#'     in the range `[0,1]` defining the minimum acceptable compressed versus
#'     the original data size. Default value of `NULL` means that the threshold
#'     will be automatically determined based on some internal heuristic
#'     (e.g., 0.01 for models without fixed effects).
#'   - `compress_nmax` defines the maximum allowable size (in rows) of the
#'     compressed dataset that can be serialized into R. Pays heed to the idea
#'     that big data serialization can be costly (esp. for remote databases),
#'     even if we have achieved good compression on top of the original dataset.
#'     Default value is 1e6 (i.e., a million rows).
#'
#'   See the Acceleration Strategies section below for further details.
#' @param query_only Logical indicating whether only the underlying compression
#'   SQL query should be returned (i.e., no computation will be performed).
#'   Default is `FALSE`.
#' @param data_only Logical indicating whether only the compressed dataset
#'   should be returned (i.e., no regression is run). Default is `FALSE`.
#' @param drop_missings Logical indicating whether incomplete cases (i.e., rows
#'   where any of the dependent, independent or FE variables are
#'   missing) should be dropped. The default is `TRUE`, according with standard
#'   regression software. It is *strongly* recommended not to change this value
#'   unless you are absolutely sure that your data have no missings and you wish
#'   to skip some internal checks. (Even then, it probably isn't worth it.)
#' @param verbose Logical. Print auto strategy and progress messages to the
#'   console? Defaults to `TRUE`.
#'
#' @return A list of class "dbreg" containing various slots, including a table
#' of coefficients (which the associated print method will display).
#'
#' @section Acceleration Strategies:
#'
#' `dbreg` offers four primary acceleration strategies for estimating regression
#' results from simplified data representations. Below we use the shorthand
#' Y (outcome), X (explanatory variables), and FE (fixed effects) for exposition
#' purposes:
#'
#' 1. `"compress"`: compresses the data via a `GROUP BY` operation (using X and
#'    the FE as groups), before running weighted least squares on this much
#'    smaller dataset:
#'    \deqn{\hat{\beta} = (X_c' W X_c)^{-1} X_c' W Y_c}
#'    where \eqn{W = \text{diag}(n_g)} are the group frequencies. This procedure
#'    follows Wang et al. (2021).
#' 2. `"moments"`: computes sufficient statistics (\eqn{X'X, X'y}) directly via
#'    SQL aggregation, returning a single-row result. This solves the standard
#'    OLS normal equations \eqn{\hat{\beta} = (X'X)^{-1}X'y}. Limited to cases
#'    without FE.
#' 3. `"demean"` (alias `"within"`): subtracts group-level means from both Y and
#'    X before computing sufficient statistics (per the `"moments"` strategy).
#'    For example, given unit \eqn{i} and time \eqn{t} FE, we apply double
#'    demeaning:
#'    \deqn{\ddot{Y}_{it} = \beta \ddot{X}_{it} + \varepsilon_{it}}
#'    where \eqn{\ddot{X} = X - \bar{X}_i - \bar{X}_t + \bar{X}}. This
#'    (single-pass) within transformation is algebraically equivalent to the
#'    fixed effects projection---i.e., Frisch-Waugh-Lovell partialling out---in
#'    the presence or a single FE. It is also identical for the two-way FE
#'    (TWFE) case if your panel is balanced. For unbalanced two-way panels,
#'    however, the double demeaning strategy is not algebraically equivalent to
#'    the fixed effects projection and therefore does not recover the exact TWFE
#'    coefficients. Moreover, note that this `"demean"` strategy permits at most
#'    two FE.
#' 4. `"mundlak"`: a generalized Mundlak (1978), or correlated random effects
#'    (CRE) estimator that regresses Y on X plus group means of X:
#'    \deqn{Y_{it} = \alpha + \beta X_{it} + \gamma \bar{X}_i + \varepsilon_{it} \quad \text{(one-way)}}
#'    \deqn{Y_{it} = \alpha + \beta X_{it} + \gamma \bar{X}_{i} + \delta \bar{X}_{t} + \varepsilon_{it} \quad \text{(two-way, etc.)}}
#'    Unlike `"demean"`, Y is not transformed, so predictions are on the
#'    original scale. Supports any number of FE and works correctly for any
#'    panel structure (balanced or unbalanced). However, note that CRE is a
#'    *different model* from FE: while coefficients are asymptotically
#'    equivalent under certain assumptions, they will generally differ in
#'    finite samples.
#'
#' The relative efficiency of each of these strategies depends on the size and
#' structure of the data, as well the number of unique regressors and FE. For
#' (quote unquote) "standard" cases, the `"compress"` strategy can yield
#' remarkable performance gains and should justifiably be viewed as a good
#' default. However, the compression approach tends to be less efficient for
#' true panels (repeated cross-sections over time), where N >> T. In such
#' cases, it can be more efficient to use a demeaning strategy that first
#' controls for (e.g. subtracts) group means, before computing sufficient
#' statistics on the aggregated data. The reason for this is that time and unit
#' FE are typically high dimensional, but covariate averages are not; see
#' Arkhangelsky & Imbens (2024).
#' 
#' However, the demeaning approaches invite tradeoffs of their own. For example,
#' the double demeaning transformation of the `"demean"` strategy does not
#' obtain exact TWFE results in unbalanced panels, and it is also limited to at
#' most two FE. Conversely, the `"mundlak"` (CRE) strategy obtains consistent
#' coefficients regardless of panel structure and FE count, but at the "cost" of
#' recovering a different estimand. (It is a different model to TWFE, after
#' all.)
#' 
#' Users should weigh these tradeoffs when choosing their accleration strategy.
#' Summarising, we can provide a few guiding principles. `"compress"` is a good
#' default that guarantees the "exact" FE estimates and is usually very 
#' efficient (barring data I/O costs and high FE dimensionality). `"mundlak"` is
#' another efficient alternative provided that the CRE estimand is acceptable
#' (don't be alarmed if your coefficients are not identical). Finally, the
#' `"demean"` and `"moments"` strategies are great for particular use cases
#' (i.e., balanced panels and cases without FE, respectively).
#' 
#' If this all sounds like too much to think about, don't fret. The good news
#' is that `dbreg` can do a lot (all?) of the deciding for you. Specifically, it
#' will invoke an `"auto"` heuristic behind the scenes if a user does not
#' provide an explicit acceleration strategy. Working through the heuristic
#' logic does impose some additional overhead, but this should be negligible in
#' most cases (certainly compared to the overall time savings). The `"auto"`
#' heuristic is as follows:
#'
#' - IF no FE AND (any continuous regressor OR poor compression ratio OR too big
#'   compressed data) THEN `"moments"`.
#' - ELSE IF 1 FE AND (poor compression ratio OR too big compressed data) THEN
#'   `"demean"`.
#' - ELSE IF 2 FE AND (poor compression ratio OR too big compressed data):
#'   - IF balanced panel THEN `"demean"`.
#'   - ELSE error (exact TWFE infeasible; user must explicitly choose
#'     `"compress"` or `"mundlak"`).
#' - ELSE THEN `"compress"`.
#' 
#' _Tip: set `dbreg(..., verbose = TRUE)` to print information about the auto
#' strategy decision criteria._
#'
#' @references
#' Arkhangelsky, D. & Imbens, G. (2024)
#' \cite{Fixed Effects and the Generalized Mundlak Estimator}.
#' The Review of Economic Studies, 91(5), pp. 2545–2571.
#' Available: https://doi.org/10.1093/restud/rdad089
#'
#' Mundlak, Y. (1978)
#' \cite{On the Pooling of Time Series and Cross Section Data}.
#' Econometrica, 46(1), pp. 69–85.
#' Available: https://doi.org/10.2307/1913646
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
  strategy = c("auto", "compress", "moments", "demean", "within", "mundlak"),
  compress_ratio = NULL,
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
    # true Mundlak/CRE: Y ~ X + group means of X
    "mundlak" = execute_mundlak_strategy(inputs),
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

  # compression ratio sanity check
  if (is.null(compress_ratio)) {
    # Stricter compress_ratio logic for 1 and 2 FE cases
    compress_ratio = if (length(fes) %in% 1:2) 0.6 else 0.01
  } else if (!(is.numeric(compress_ratio) && compress_ratio >= 0 && compress_ratio <= 1)) {
    stop("Argument `compress_ratio` ratio must be a numeric in the range [0, 1]\n.")
  }

  # Filter missing cases
  if (isTRUE(drop_missings)) {
    # Wrap in subquery if path already contains WHERE clause
    if (grepl("WHERE", from_statement, ignore.case = TRUE)) {
      from_statement = glue("FROM (SELECT * {from_statement}) AS subq")
    }
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

    if (verbose) {
      data_msg = paste0(
        "        - ", 
        "data has ",
        format(total_n, big.mark = ","), " rows"
      )
      if (length(fes) && !is.na(n_groups_fe)) {
        data_msg = paste0(
          data_msg,
          " with ",
          length(fes), " FE ",
          "(", format(n_groups_fe, big.mark = ","), " unique groups)"
        )
      } else if (length(fes) == 0) {
        data_msg = paste0(data_msg, " with 0 FE")
      }
      message(data_msg)
    }

    comp_rat = n_groups_total / max(total_n, 1)
    attr(comp_rat, "comp_size") = n_groups_total

    return(comp_rat)
  }

  chosen_strategy = strategy
  est_cr = NA_real_

  # Auto logic
  if (strategy == "auto") {
    if (verbose) {
      message("[dbreg] Auto strategy:")
    }
    est_cr = tryCatch(estimate_compression(inputs), error = function(e) {
      NA_real_
    })
    comp_size = attr(est_cr, "comp_size")
    fail_compress_ratio = !is.na(est_cr) && est_cr > compress_ratio
    fail_compress_nmax = !is.na(est_cr) && comp_size > compress_nmax

    if (verbose) {
      compress_ratio_msg_sign = if (fail_compress_ratio) " exceeds " else " satisfies "
      message(paste0(
        "        - ",
        "compression ratio (", sprintf("%.2f", est_cr), ")",
        compress_ratio_msg_sign,
        "threshold (", compress_ratio, ")"
      ))
      # only print compress nmax message if it fails (edge case)
      if (fail_compress_nmax) {
        compress_nmax_msg_sign = if (fail_compress_nmax) " exceeds " else " satisfies "
        message(paste0(
          "        - ",
          "compressed data size (", prettyNum(comp_size, big.mark = ","), " rows)",
          compress_nmax_msg_sign,
          "threshold (", prettyNum(compress_nmax, big.mark = ","), " rows)"
        ))
      }
    }

    if (length(fes) == 0) {
      if (verbose) {
        if (any_continuous) {
          message("        - continuous variables detected")
        }
      }
      if (any_continuous || (fail_compress_ratio || fail_compress_nmax)) {
        chosen_strategy = "moments"
      } else {
        chosen_strategy = "compress"
      }
    } else if (length(fes) %in% c(1, 2)) {
      if (fail_compress_ratio || fail_compress_nmax) {
        # For 2-way FE, check balance
        if (length(fes) == 2) {
          fe_expr = paste(fes, collapse = ", ")
          balance_sql = glue(
            "SELECT COUNT(DISTINCT cnt) AS n FROM (SELECT COUNT(*) AS cnt {from_statement} GROUP BY {fe_expr}) t"
          )
          is_balanced = tryCatch(dbGetQuery(conn, balance_sql)$n == 1, error = function(e) NA)
          if (isTRUE(is_balanced)) {
            chosen_strategy = "demean"
            if (verbose) {
              message("        - panel is balanced")
            }
          } else {
            stop(
              "[dbreg] Exact TWFE infeasible for unbalanced panel under current transfer limits.\n\n",
              "Users have two recommended options:\n",
              "  - strategy = 'compress' with less strict compression thresholds (for exact TWFE), or\n",
              "  - strategy = 'mundlak' (for CRE estimator; different model so requires explicit opt-in)",
              call. = FALSE
            )
          }
        } else {
          chosen_strategy = "demean"
        }
      } else {
        chosen_strategy = "compress"
      }
    } else {
      # browser()
      # > 3 FEs, default to compress
      if (verbose) {
        message("        - more than 2 FEs")
      }
      chosen_strategy = "compress"
    }
    if (verbose) {
      message("        - decision: ", chosen_strategy)
    }
    } else {
    chosen_strategy = strategy
    if (verbose) {
      message("[dbreg] Using strategy: ", chosen_strategy)
    }
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
      message("[dbreg] demean requires <= 2 FEs. Using compress.")
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
  
  # CTE structure for HC1 meat computation
  cte_sql = paste0("WITH base AS (SELECT * ", inputs$from_statement, ")")
  
  moments_sql = paste0(
    cte_sql, "\n",
    "SELECT\n  ",
    paste(pair_exprs, collapse = ",\n  "),
    "\nFROM base"
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
  
  # Compute HC1 meat matrix if needed
  meat = NULL
  if (inputs$vcov_type_req == "hc1") {
    is_athena = inherits(inputs$conn, "AthenaConnection")
    meat = compute_meat_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = inputs$xvars,
      yvar = inputs$yvar,
      betahat = betahat,
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "base",
      has_intercept = TRUE
    )
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "moments",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    meat = meat
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
  all_vars = c(inputs$yvar, inputs$xvars)
  
  if (length(inputs$fes) == 1) {
    # Single FE: simple within-group demeaning
    fe1 = inputs$fes[1]

    means_cols = paste(
      sprintf("AVG(%s) AS %s_mean", all_vars, all_vars),
      collapse = ", "
    )
    tilde_exprs = paste(
      sprintf("(b.%s - gm.%s_mean) AS %s_tilde", all_vars, all_vars, all_vars),
      collapse = ",\n       "
    )

    # CTE part (reusable for HC1 meat computation)
    cte_sql = paste0(
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
      )"
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
  } else {
    # Two FE: double demeaning
    fe1 = inputs$fes[1]
    fe2 = inputs$fes[2]

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

    # CTE part (reusable for HC1 meat computation)
    cte_sql = paste0(
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
      )"
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
  }

  # Add moment terms for xvars (shared by both 1-FE and 2-FE cases)
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

  # Build full SQL
  demean_sql = paste0(
    cte_sql,
    ",
      moments AS (
      SELECT
          ",
    paste(moment_terms, collapse = ",\n    "),
    "
      FROM demeaned
      )
      SELECT * FROM moments"
  )

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
  
  # Compute HC1 meat matrix if needed
  meat = NULL
  if (inputs$vcov_type_req == "hc1") {
    is_athena = inherits(inputs$conn, "AthenaConnection")
    meat = compute_meat_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = inputs$xvars,
      yvar = inputs$yvar,
      betahat = betahat,
      is_athena = is_athena
    )
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "demean",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    meat = meat
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

#' Execute true Mundlak/CRE strategy
#'
#' Regresses Y on X plus group means of X for each fixed effect.
#' Y is NOT demeaned - predictions are on the original scale.
#'
#' @keywords internal
execute_mundlak_strategy = function(inputs) {
  xvars = inputs$xvars
  yvar = inputs$yvar
  fes = inputs$fes
  n_fes = length(fes)

  if (n_fes == 0) {
    stop("mundlak strategy requires at least one fixed effect")
  }

  # Build group means CTEs and join clauses for each FE
  cte_parts = character(0)
  join_parts = character(0)
  xbar_all = character(0)

  for (k in seq_along(fes)) {
    fe_k = fes[k]
    suffix = paste0("_bar_", fe_k)
    xbar_k = paste0(xvars, suffix)
    xbar_all = c(xbar_all, xbar_k)

    means_cols = paste(sprintf("AVG(%s) AS %s", xvars, xbar_k), collapse = ", ")
    cte_parts = c(cte_parts, sprintf(
      "fe%d_means AS (SELECT %s, %s FROM base GROUP BY %s)",
      k, fe_k, means_cols, fe_k
    ))
    join_parts = c(join_parts, sprintf(
      "JOIN fe%d_means m%d ON b.%s = m%d.%s",
      k, k, fe_k, k, fe_k
    ))
  }

  # Select columns for augmented table
  aug_select_parts = "b.*"
  for (k in seq_along(fes)) {
    suffix = paste0("_bar_", fes[k])
    xbar_k = paste0(xvars, suffix)
    aug_select_parts = c(aug_select_parts, paste0("m", k, ".", xbar_k))
  }
  aug_select = paste(aug_select_parts, collapse = ", ")

  # All regressors: original X plus all group means
  all_regressors = c(xvars, xbar_all)

  # Build moment terms
  moment_terms = c(
    sql_count(inputs$conn, "n_total"),
    if (n_fes >= 1) sql_count(inputs$conn, "n_fe1", fes[1], distinct = TRUE) else "1 AS n_fe1",
    if (n_fes >= 2) sql_count(inputs$conn, "n_fe2", fes[2], distinct = TRUE) else "1 AS n_fe2",
    sprintf("SUM(CAST(%s AS FLOAT)) AS sum_y", yvar),
    sprintf("SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS sum_y_sq", yvar, yvar)
  )

  # sum(X_j) and sum(X_j * Y) for each regressor

  for (v in all_regressors) {
    moment_terms = c(
      moment_terms,
      sprintf("SUM(CAST(%s AS FLOAT)) AS sum_%s", v, v),
      sprintf("SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS sum_%s_%s", v, yvar, v, yvar)
    )
  }

  # sum(X_i * X_j) for all pairs (upper triangle including diagonal)
  for (i in seq_along(all_regressors)) {
    for (j in i:length(all_regressors)) {
      vi = all_regressors[i]
      vj = all_regressors[j]
      moment_terms = c(
        moment_terms,
        sprintf("SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS sum_%s_%s", vi, vj, vi, vj)
      )
    }
  }

  # CTE part (reusable for HC1 meat computation)
  cte_sql = paste0(
    "WITH base AS (SELECT * ", inputs$from_statement, "),\n",
    paste(cte_parts, collapse = ",\n"), ",\n",
    "augmented AS (SELECT ", aug_select, " FROM base b ", paste(join_parts, collapse = " "), ")"
  )

  mundlak_sql = paste0(
    cte_sql, ",\n",
    "moments AS (SELECT ", paste(moment_terms, collapse = ", "), " FROM augmented)\n",
    "SELECT * FROM moments"
  )

  # Athena FLOAT gotcha
  if (inherits(inputs$conn, "AthenaConnection")) {
    mundlak_sql = gsub("FLOAT", "REAL", mundlak_sql, fixed = TRUE)
  }

  if (inputs$query_only) {
    return(mundlak_sql)
  }

  if (inputs$verbose) {
    message("[dbreg] Executing mundlak SQL\n")
  }
  mundlak_df = dbGetQuery(inputs$conn, mundlak_sql)
  if (inputs$data_only) {
    return(mundlak_df)
  }

  n_total = mundlak_df$n_total
  n_fe1 = mundlak_df$n_fe1
  n_fe2 = mundlak_df$n_fe2

  # Include intercept
  vars_all = c("(Intercept)", all_regressors)
  p = length(vars_all)

  XtX = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  Xty = matrix(0, p, 1, dimnames = list(vars_all, ""))

  # Intercept terms
  XtX["(Intercept)", "(Intercept)"] = n_total
  Xty["(Intercept)", ] = mundlak_df$sum_y

  # Regressor terms
  for (v in all_regressors) {
    XtX["(Intercept)", v] = XtX[v, "(Intercept)"] = mundlak_df[[paste0("sum_", v)]]
    XtX[v, v] = mundlak_df[[paste0("sum_", v, "_", v)]]
    Xty[v, ] = mundlak_df[[paste0("sum_", v, "_", yvar)]]
  }

  # Cross-terms
  for (i in seq_along(all_regressors)) {
    for (j in seq_along(all_regressors)) {
      if (i < j) {
        vi = all_regressors[i]
        vj = all_regressors[j]
        XtX[vi, vj] = XtX[vj, vi] = mundlak_df[[paste0("sum_", vi, "_", vj)]]
      }
    }
  }

  solve_result = solve_with_fallback(XtX, Xty)
  betahat = solve_result$betahat
  XtX_inv = solve_result$XtX_inv
  rownames(betahat) = vars_all

  # RSS and TSS
  rss = as.numeric(
    mundlak_df$sum_y_sq -
      2 * t(betahat) %*% Xty +
      t(betahat) %*% XtX %*% betahat
  )
  tss = mundlak_df$sum_y_sq - (mundlak_df$sum_y^2 / n_total)

  df_res = max(n_total - p, 1)

  # Compute HC1 meat matrix if needed
  meat = NULL
  if (inputs$vcov_type_req == "hc1") {
    is_athena = inherits(inputs$conn, "AthenaConnection")
    meat = compute_meat_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = all_regressors,
      yvar = yvar,
      betahat = betahat,
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "augmented",
      has_intercept = TRUE
    )
  }

  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "mundlak",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = inputs$fml,
    yvar = yvar,
    xvars = xvars,
    fes = fes,
    query_string = mundlak_sql,
    nobs = 1L,
    nobs_orig = n_total,
    strategy = "mundlak",
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
    warning(paste0(
      sprintf(
        "[dbreg] compression ineffective (%.1f%% of original rows). ",
        100 * compression_ratio
      ),
      "Consider strategy = 'mundlak'."
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
  rss_g = NULL,
  meat = NULL
) {
  if (vcov_type == "hc1") {
    if (strategy == "compress") {
      # Compress strategy: HC1 with grouped residuals
      meat = crossprod(X, Diagonal(x = as.numeric(rss_g)) %*% X)
    }
    # meat should be provided for demean/mundlak/moments strategies
    if (is.null(meat)) {
      stop("HC1 requires meat matrix for non-compress strategies")
    }
    scale_hc1 = nobs_orig / df_res
    vcov_mat = scale_hc1 * (XtX_inv %*% meat %*% XtX_inv)
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

#' Compute HC1 meat matrix via SQL
#' @keywords internal
compute_meat_sql = function(conn, cte_sql, vars, yvar, betahat, 
                            is_athena = FALSE, 
                            var_suffix = "_tilde",
                            cte_name = "demeaned",
                            has_intercept = FALSE) {
  # Build variable names with suffix
  vars_sql = paste0(vars, var_suffix)
  yvar_sql = paste0(yvar, var_suffix)
  
  # Extract beta values (betahat may be a matrix)
  beta_vals = as.numeric(betahat[vars, 1])
  
  # Build residual expression: y - intercept - sum(beta_j * x_j)
  if (has_intercept) {
    intercept_val = as.numeric(betahat["(Intercept)", 1])
    beta_terms = paste(
      sprintf("%.15g * %s", beta_vals, vars_sql),
      collapse = " + "
    )
    resid_expr = sprintf("(%s - %.15g - (%s))", yvar_sql, intercept_val, beta_terms)
  } else {
    beta_terms = paste(
      sprintf("%.15g * %s", beta_vals, vars_sql),
      collapse = " + "
    )
    resid_expr = sprintf("(%s - (%s))", yvar_sql, beta_terms)
  }
  
  # Build meat terms: SUM(e^2 * xi * xj) for all pairs including diagonal
  # If has_intercept, include intercept as first "variable" (value = 1)
  meat_terms = character(0)
  
  if (has_intercept) {
    # Intercept-intercept term: SUM(e^2 * 1 * 1) = SUM(e^2)
    meat_terms = c(meat_terms, sprintf(
      "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS meat_intercept_intercept",
      resid_expr, resid_expr
    ))
    # Intercept-variable terms: SUM(e^2 * 1 * xj) = SUM(e^2 * xj)
    for (j in seq_along(vars)) {
      vj = vars[j]
      vj_sql = paste0(vj, var_suffix)
      meat_terms = c(meat_terms, sprintf(
        "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS meat_intercept_%s",
        resid_expr, resid_expr, vj_sql, vj
      ))
    }
  }
  
  # Variable-variable terms
  for (i in seq_along(vars)) {
    for (j in i:length(vars)) {
      vi = vars[i]
      vj = vars[j]
      vi_sql = paste0(vi, var_suffix)
      vj_sql = paste0(vj, var_suffix)
      meat_terms = c(meat_terms, sprintf(
        "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT) * CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS meat_%s_%s",
        resid_expr, resid_expr, vi_sql, vj_sql, vi, vj
      ))
    }
  }
  
  meat_sql = paste0(
    cte_sql,
    ",\nmeat AS (SELECT ", paste(meat_terms, collapse = ", "), " FROM ", cte_name, ")\n",
    "SELECT * FROM meat"
  )
  
  if (is_athena) {
    meat_sql = gsub("FLOAT", "REAL", meat_sql, fixed = TRUE)
  }
  
  meat_df = dbGetQuery(conn, meat_sql)
  
  # Reconstruct meat matrix
  if (has_intercept) {
    vars_all = c("(Intercept)", vars)
  } else {
    vars_all = vars
  }
  p = length(vars_all)
  meat_mat = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  
  if (has_intercept) {
    meat_mat["(Intercept)", "(Intercept)"] = meat_df$meat_intercept_intercept
    for (j in seq_along(vars)) {
      vj = vars[j]
      val = meat_df[[sprintf("meat_intercept_%s", vj)]]
      meat_mat["(Intercept)", vj] = meat_mat[vj, "(Intercept)"] = val
    }
  }
  
  for (i in seq_along(vars)) {
    for (j in i:length(vars)) {
      vi = vars[i]
      vj = vars[j]
      val = meat_df[[sprintf("meat_%s_%s", vi, vj)]]
      meat_mat[vi, vj] = meat_mat[vj, vi] = val
    }
  }
  meat_mat
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
