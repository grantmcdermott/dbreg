#' Run a regression on a database backend
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
#' `fml = y ~ x1 + x2 | fe1 + f2`. Interaction terms are supported using
#' standard R syntax (`:` for interactions, `*` for main effects plus
#' interaction). Transformations and literals are not yet supported.
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
#' @param vcov Character string or formula denoting the desired type of variance-
#' covariance correction / standard errors. Options are `"iid"` (default),
#' `"hc1"` (heteroskedasticity-consistent), or a one-sided formula like
#' `~cluster_var` for cluster-robust standard errors. Note that `"hc1"` and
#' clustered SEs require a second pass over the data unless
#' `strategy = "compress"` to construct the residuals.
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
#' @param cluster Optional. Provides an alternative way to specify
#' cluster-robust standard errors (i.e., instead of `vcov = ~cluster_var`).
#' Either a one-sided formula (e.g., `~firm`) or character string giving the
#' variable name. Only single-variable clustering is currently supported.
#' @param ssc Character string controlling the small-sample correction for
#' clustered standard errors. Options are `"full"` (default) or `"nested"`.
#' With `"full"`, all parameters (including fixed effect dummies) are counted
#' in K for the CR1 correction. With `"nested"`, fixed effects that are nested
#' within the cluster variable are excluded from K, matching the default
#' behavior of `fixest::feols`. Only applies to `"compress"` and `"demean"`
#' strategies (Mundlak uses explicit group mean regressors, not FE dummies).
#' This distinction only matters for small samples. For large datasets
#' (`dbreg`'s target use case), the difference is negligible and hence we
#' default to the simple `"full"` option.
#' @param sql_only Logical indicating whether only the underlying compression
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
#'   console? Defaults to `FALSE`. This can be overridden for a single call
#'   by supplying `verbose = TRUE`, or set globally via
#'   `options(dbreg.verbose = TRUE)`.
#' @param ... Additional arguments. Currently ignored, except to handle
#'   superseded arguments for backwards compatibility.
#'
#' @return A list of class "dbreg" containing various slots, including a table
#' of coefficients (which the associated `print` method will display).
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
#'    the presence of a single FE. It is also identical for the two-way FE
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
#' all.) See Wooldridge (2025) for an extended discussion of these issues.
#' 
#' Users should weigh these tradeoffs when choosing their acceleration strategy.
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
#' Wooldridge, J.M. (2025)
#' \cite{Two-way fixed effects, the two-way mundlak regression, and difference-in-differences estimators}.
#' Empirical Economics, 69, pp. 2545–2587.
#' Available: https://doi.org/10.1007/s00181-025-02807-z
#'
#' @seealso \code{\link[DBI]{dbConnect}} for creating database connections,
#' \code{\link[duckdb]{duckdb}} for DuckDB-specific connections
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetInfo dbGetQuery dbIsValid dbRemoveTable dbWriteTable
#' @importFrom duckdb duckdb duckdb_register duckdb_unregister
#' @importFrom Formula Formula
#' @importFrom Matrix chol2inv crossprod Diagonal sparse.model.matrix
#' @importFrom stats aggregate as.formula formula pt reformulate setNames
#' @importFrom glue glue glue_sql
#'
#' @examples
#' #
#' ## In-memory data ----
#' 
#' # We can pass in-memory R data frames to an ephemeral DuckDB connection via
#' # the `data` argument. This is convenient for small(er) datasets and demos.
#'
#' # Default "compress" strategy reduces the data to 4 rows before running OLS
#' dbreg(weight ~ Diet, data = ChickWeight)
#' 
#' # Compare with lm
#' summary(lm(weight ~ Diet, data = ChickWeight))$coefficients
#' 
#' # Add "fixed effects" after a `|` 
#' dbreg(weight ~ Time | Diet, data = ChickWeight)
#' 
#' # "robust" SEs can also be computed using a sufficient statistics approach
#' dbreg(weight ~ Time | Diet, data = ChickWeight, vcov = "hc1")
#' dbreg(weight ~ Time | Diet, data = ChickWeight, vcov = ~Chick)
#' 
#' # Different acceleration strategies + specifications
#' dbreg(weight ~ Time | Diet, data = ChickWeight, strategy = "demean")
#' dbreg(weight ~ Time | Diet, data = ChickWeight, strategy = "mundlak")
#' dbreg(weight ~ Time | Diet + Chick, data = ChickWeight, strategy = "mundlak") # two-way Mundlak
#' dbreg(weight ~ Time, data = ChickWeight, strategy = "moments") # no FEs
#' # etc.
#' 
#' #
#' ## DBI connection ----
#' 
#' # For persistent databases or more control, use the `conn` + `table` args.
#' # Again, we use DuckDB below but any other DBI-supported backend should work
#' # too (e.g., odbc, bigrquery, noctua (AWS Athena),  etc.) See:
#' # https://r-dbi.org/backends/
#' 
#' library(DBI)
#' con = dbConnect(duckdb::duckdb())
#' dbWriteTable(con, "cw", as.data.frame(ChickWeight))
#' 
#' dbreg(weight ~ Time | Diet, conn = con, table = "cw")
#' 
#' # Tip: Rather than creating or writing (temp) tables, use CREATE VIEW to
#' # define subsets or computed columns without materializing data. This is more
#' # efficient and especially useful for filtering or adding variables.
#' dbExecute(
#'   con,
#'   "
#'   CREATE VIEW cw1 AS
#'   SELECT *
#'   FROM cw
#'   WHERE Diet = 1
#'   "
#' )
#' dbreg(weight ~ Time | Chick, conn = con, table = "cw1")
#' 
#' #
#' ## Path to file ----
#' #
#' # For file-based data (e.g., parquet), use the path argument.
#' 
#' tmp = tempfile(fileext = ".parquet")
#' dbExecute(con, sprintf("COPY cw TO '%s' (FORMAT PARQUET)", tmp))
#' 
#' dbreg(weight ~ Time | Diet, path = tmp)
#' 
#' # Cleanup
#' dbDisconnect(con)
#' unlink(tmp)
#' 
#' #
#' ## Big dataset ----
#' 
#' # For a more compelling and appropriate dbreg use-case, i.e. regression on a
#' # big (~180 million row) dataset of Hive-partioned parquet files, see the
#' # package website:
#' # https://grantmcdermott.com/dbreg/
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
  cluster = NULL,
  ssc = c("full", "nested"),
  sql_only = FALSE,
  data_only = FALSE,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {

  verbose = isTRUE(verbose)
  ssc = match.arg(ssc)
  # Parse vcov: can be string or formula (for clustering)
  # Check formula first before any string operations
  if (inherits(vcov, "formula")) {
    cluster = vcov
    vcov = "cluster"
  } else if (is.character(vcov)) {
    vcov = tolower(vcov[1])
    vcov = match.arg(vcov, c("iid", "hc1"))
  } else {
    stop("vcov must be a character string ('iid', 'hc1') or a formula for clustering")
  }
  # Parse cluster argument
  if (!is.null(cluster)) {
    if (inherits(cluster, "formula")) {
      cluster_vars = all.vars(cluster)
      if (length(cluster_vars) != 1) {
        stop("Only single-variable clustering is currently supported")
      }
      cluster = cluster_vars
    }
    vcov = "cluster"
  }
  strategy = match.arg(strategy)
  if (strategy == "within") strategy = "demean"  # alias

  # superseded args handled through ...
  dots = list(...)
  if (length(dots)) {
    if (!is.null(dots[["query_only"]]) && !identical(sql_only, dots[["query_only"]])) {
      sql_only = dots[["query_only"]]
      warning("The `query_only` argument has been superseded by `sql_only` and will be deprecated in a future `dbreg` release.\n")
    }
  }

  # Process and validate inputs
  inputs = process_dbreg_inputs(
    fml = fml,
    conn = conn,
    table = table,
    data = data,
    path = path,
    vcov = vcov,
    cluster = cluster,
    ssc = ssc,
    strategy = strategy,
    sql_only = sql_only,
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
  cluster,
  ssc,
  strategy,
  sql_only,
  data_only,
  compress_ratio,
  compress_nmax,
  drop_missings,
  verbose
) {
  vcov_type_req = vcov
  cluster_var = cluster

  own_conn = FALSE

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
  }

  # FROM clause
  if (!is.null(table)) {
    if (is.character(table)) {
      # Original behavior: table name
      from_statement = glue("FROM {table}")
    } else if (inherits(table, "tbl_lazy")) {
      # lazy table: render SQL
      rendered_sql = tryCatch(dbplyr::sql_render(table), error = function(e) {
        NULL
      })
      if (is.null(rendered_sql)) {
        stop("Failed to render SQL for provided tbl_lazy.")
      }
      from_statement = paste0("FROM (", rendered_sql, ") AS lazy_subquery")
      # Connection should already be set (either explicitly or inferred above)
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
    temp_name = sprintf("tmp_table_dbreg_%s", 
                       gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")))
    duckdb_register(conn, temp_name, data)
    from_statement = paste("FROM", temp_name)
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

  # Get term structure (preserves interactions)
  rhs1 = formula(fml, lhs = 0, rhs = 1)
  tt = terms(rhs1)
  term_labels = attr(tt, "term.labels")
  xvars = all.vars(rhs1)  # unique variable names (for column validation)
  has_interactions = any(grepl(":", term_labels))
  
  fe = if (length(fml)[2] > 1) {
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
    if (is.factor(xv) || is.character(xv) || is.logical(xv)) {
      return(FALSE)
    }
    if (is.integer(xv)) {
      return(FALSE)
    }
    if (is.numeric(xv)) {
      return(length(unique(xv)) > min(50, 0.2 * length(xv)))
    }
    FALSE
  }
  any_continuous = if (!is.null(data)) {
    any(vapply(xvars, is_continuous, logical(1)))
  } else {
    FALSE
  }

  # compression ratio sanity check
  if (is.null(compress_ratio)) {
    # Stricter compress_ratio logic for 1 and 2 FE cases
    compress_ratio = if (length(fe) %in% 1:2) 0.6 else 0.01
  } else if (!(is.numeric(compress_ratio) && compress_ratio >= 0 && compress_ratio <= 1)) {
    stop("Argument `compress_ratio` ratio must be a numeric in the range [0, 1]\n.")
  }

  # Filter missing cases
  if (isTRUE(drop_missings)) {
    # Wrap in subquery if from_statement contains clauses that must come after WHERE
    if (grepl("WHERE|LIMIT|ORDER\\s+BY|GROUP\\s+BY|HAVING", from_statement, ignore.case = TRUE)) {
      from_statement = glue("FROM (SELECT * {from_statement}) AS subq")
    }
    from_statement = glue("
    {from_statement}
    WHERE {yvar} IS NOT NULL
    AND {paste(xvars, collapse = ' IS NOT NULL AND ')} IS NOT NULL
    "
    )
    if (!is.null(fe)) {
      from_statement = glue("
      {from_statement}
      AND {paste(fe, collapse = ' IS NOT NULL AND ')} IS NOT NULL
      ")
    }
  }

  list(
    fml = fml,
    yvar = yvar,
    xvars = xvars,
    term_labels = term_labels,
    has_interactions = has_interactions,
    fe = fe,
    conn = conn,
    from_statement = from_statement,
    data = data,
    vcov_type_req = vcov_type_req,
    cluster_var = cluster_var,
    ssc = ssc,
    strategy = strategy,
    sql_only = sql_only,
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
#' @keywords internal
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
  fe = inputs$fe
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
    fe = inputs$fe
    from_statement = inputs$from_statement

    key_cols = c(xvars, fe)
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

    if (length(fe)) {
      # count unique FE groups (may be single or multi-column)
      n_groups_fe = tryCatch(count_distinct_tuples(fe), error = function(e) {
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
      if (length(fe) && !is.na(n_groups_fe)) {
        data_msg = paste0(
          data_msg,
          " with ",
          length(fe), " FE ",
          "(", format(n_groups_fe, big.mark = ","), " unique groups)"
        )
      } else if (length(fe) == 0) {
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

    if (length(fe) == 0) {
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
    } else if (length(fe) %in% c(1, 2)) {
      if (fail_compress_ratio || fail_compress_nmax) {
        # For 2-way FE, check balance
        if (length(fe) == 2) {
          fe_expr = paste(fe, collapse = ", ")
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
            if (verbose) {
              message("        - panel is unbalanced")
            }
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
  if (chosen_strategy == "moments" && length(fe) > 0) {
    warning(
      "[dbreg] FE present; moments (no-FE) not applicable. Using compress."
    )
    chosen_strategy = "compress"
  }
  if (chosen_strategy == "demean") {
    if (!(length(fe) %in% c(1, 2))) {
      warning("[dbreg] demean requires <= 2 FEs. Using compress.")
      chosen_strategy = "compress"
    } else if (verbose && length(fe) == 2) {
      # For 2-way FE, check balance; just a warning since user has explicitly selected into demean
      fe_expr = paste(fe, collapse = ", ")
      balance_sql = glue(
        "SELECT COUNT(DISTINCT cnt) AS n FROM (SELECT COUNT(*) AS cnt {from_statement} GROUP BY {fe_expr}) t"
      )
      is_balanced = tryCatch(dbGetQuery(conn, balance_sql)$n == 1, error = function(e) NA)
      if (!is_balanced) {
        warning("[dbreg] Panel appears unbalanced. Double demeaning may yield different coefficients than exact TWFE.")
      }
    }
  }
  
  # Interactions only supported with compress/moments strategies (for now)
  if (isTRUE(inputs$has_interactions) && chosen_strategy %in% c("demean", "mundlak")) {
    if (verbose) {
      message("[dbreg] Interactions detected; forcing strategy = 'compress'")
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
  # Get SQL expressions for design matrix terms
  # For interactions/factors, this expands to CASE WHEN expressions
  if (isTRUE(inputs$has_interactions)) {
    table_ref = sub("^FROM\\s+", "", inputs$from_statement, ignore.case = TRUE)
    sql_design = sql_model_matrix(
      inputs$fml,
      inputs$conn,
      table_ref,
      expand = "all"
    )
    xvars_sql = sql_design$select_exprs
    xvar_names = sql_design$col_names
  } else {
    xvars_sql = inputs$xvars
    xvar_names = inputs$xvars
  }
  
  pair_exprs = c(
    "COUNT(*) AS n_total",
    glue("SUM({inputs$yvar}) AS sum_y"),
    glue("SUM({inputs$yvar}*{inputs$yvar}) AS sum_y_sq")
  )
  for (i in seq_along(xvars_sql)) {
    x_sql = xvars_sql[i]
    x_name = xvar_names[i]
    pair_exprs = c(
      pair_exprs,
      glue("SUM({x_sql}) AS sum_{x_name}"),
      glue("SUM(({x_sql})*{inputs$yvar}) AS sum_{x_name}_y"),
      glue("SUM(({x_sql})*({x_sql})) AS sum_{x_name}_{x_name}")
    )
  }
  xpairs = gen_xvar_pairs(xvar_names)
  for (k in seq_along(xpairs)) {
    i = match(xpairs[[k]][1], xvar_names)
    j = match(xpairs[[k]][2], xvar_names)
    xi_sql = xvars_sql[i]
    xj_sql = xvars_sql[j]
    xi_name = xvar_names[i]
    xj_name = xvar_names[j]
    pair_exprs = c(pair_exprs, glue("SUM(({xi_sql})*({xj_sql})) AS sum_{xi_name}_{xj_name}"))
  }
  
  # CTE structure for HC1 meat computation
  cte_sql = paste0("WITH base AS (SELECT * ", inputs$from_statement, ")")
  
  moments_sql = paste0(
    cte_sql, "\n",
    "SELECT\n  ",
    paste(pair_exprs, collapse = ",\n  "),
    "\nFROM base"
  )

  if (inputs$sql_only) {
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

  vars_all = c("(Intercept)", xvar_names)
  p = length(vars_all)
  XtX = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  Xty = matrix(0, p, 1, dimnames = list(vars_all, ""))

  XtX["(Intercept)", "(Intercept)"] = n_total
  Xty["(Intercept)", ] = moments_df$sum_y
  for (x in xvar_names) {
    sx = moments_df[[paste0("sum_", x)]]
    sxx = moments_df[[paste0("sum_", x, "_", x)]]
    sxy = moments_df[[paste0("sum_", x, "_y")]]
    XtX["(Intercept)", x] = XtX[x, "(Intercept)"] = sx
    XtX[x, x] = sxx
    Xty[x, ] = sxy
  }
  xpairs = gen_xvar_pairs(xvar_names)
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
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = NULL
  is_athena = inherits(inputs$conn, "AthenaConnection")
  if (inputs$vcov_type_req == "hc1") {
    meat = compute_meat_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = xvar_names,
      vars_sql = xvars_sql,
      yvar = inputs$yvar,
      betahat = betahat,
      is_athena = is_athena,
      var_suffix = "",
      cte_name = "base",
      has_intercept = TRUE
    )
  } else if (inputs$vcov_type_req == "cluster") {
    meat = compute_meat_cluster_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = xvar_names,
      vars_sql = xvars_sql,
      yvar = inputs$yvar,
      betahat = betahat,
      cluster_var = inputs$cluster_var,
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
    n_params = p,
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
    fe = NULL,
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
  # Interactions not yet supported for demean strategy
  if (isTRUE(inputs$has_interactions)) {
    stop("Interaction terms are not yet supported with strategy = 'demean'. ",
         "Use strategy = 'compress' instead.")
  }
  
  all_vars = c(inputs$yvar, inputs$xvars)
  
  if (length(inputs$fe) == 1) {
    # Single FE: simple within-group demeaning
    fe1 = inputs$fe[1]

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
    fe1 = inputs$fe[1]
    fe2 = inputs$fe[2]

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

  if (inputs$sql_only) {
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
  
  # Compute meat matrix if needed (HC1 or cluster)
  meat = NULL
  n_params_cluster = p + df_fe  # K for CR1 correction
  is_athena = inherits(inputs$conn, "AthenaConnection")
  if (inputs$vcov_type_req == "hc1") {
    meat = compute_meat_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = inputs$xvars,
      yvar = inputs$yvar,
      betahat = betahat,
      is_athena = is_athena
    )
  } else if (inputs$vcov_type_req == "cluster") {
    meat = compute_meat_cluster_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = inputs$xvars,
      yvar = inputs$yvar,
      betahat = betahat,
      cluster_var = inputs$cluster_var,
      is_athena = is_athena
    )
    # For ssc = "nested", exclude nested FE levels from K
    if (inputs$ssc == "nested") {
      nested_levels = count_nested_fe_levels(
        inputs$conn, inputs$from_statement, inputs$fe, inputs$cluster_var
      )
      n_params_cluster = p + df_fe - nested_levels
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "demean",
    XtX_inv = XtX_inv,
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    n_params = n_params_cluster,
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
    fe = inputs$fe,
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
  # Interactions not yet supported for mundlak strategy
  if (isTRUE(inputs$has_interactions)) {
    stop("Interaction terms are not yet supported with strategy = 'mundlak'. ",
         "Use strategy = 'compress' instead.")
  }
  
  xvars = inputs$xvars
  yvar = inputs$yvar
  fe = inputs$fe
  n_fe = length(fe)

  if (n_fe == 0) {
    stop("mundlak strategy requires at least one fixed effect")
  }

  # Build group means CTEs and join clauses for each FE
  cte_parts = character(0)
  join_parts = character(0)
  xbar_all = character(0)

  for (k in seq_along(fe)) {
    fe_k = fe[k]
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
  for (k in seq_along(fe)) {
    suffix = paste0("_bar_", fe[k])
    xbar_k = paste0(xvars, suffix)
    aug_select_parts = c(aug_select_parts, paste0("m", k, ".", xbar_k))
  }
  aug_select = paste(aug_select_parts, collapse = ", ")

  # All regressors: original X plus all group means
  all_regressors = c(xvars, xbar_all)

  # Build moment terms
  moment_terms = c(
    sql_count(inputs$conn, "n_total"),
    if (n_fe >= 1) sql_count(inputs$conn, "n_fe1", fe[1], distinct = TRUE) else "1 AS n_fe1",
    if (n_fe >= 2) sql_count(inputs$conn, "n_fe2", fe[2], distinct = TRUE) else "1 AS n_fe2",
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

  if (inputs$sql_only) {
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

  # Compute meat matrix if needed (HC1 or cluster)
  meat = NULL
  is_athena = inherits(inputs$conn, "AthenaConnection")
  if (inputs$vcov_type_req == "hc1") {
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
  } else if (inputs$vcov_type_req == "cluster") {
    meat = compute_meat_cluster_sql(
      conn = inputs$conn,
      cte_sql = cte_sql,
      vars = all_regressors,
      yvar = yvar,
      betahat = betahat,
      cluster_var = inputs$cluster_var,
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
    n_params = p,
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
    fe = fe,
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

  # Handle interactions: expand to SQL expressions
  if (isTRUE(inputs$has_interactions)) {
    # Extract table name from FROM statement for sql_model_matrix
    table_ref = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
    
    # Get SQL expansions for RHS terms (expand interactions only, keep main effects as-is)
    sql_design = sql_model_matrix(
      inputs$fml,
      inputs$conn,
      table_ref,
      expand = "interactions"
    )
    
    # Build SELECT expressions with aliases
    select_exprs = paste0(sql_design$select_exprs, " AS ", sql_design$col_names)
    xvars_sql = paste(select_exprs, collapse = ", ")
    xvar_names = sql_design$col_names
  } else {
    xvars_sql = paste(inputs$xvars, collapse = ", ")
    xvar_names = inputs$xvars
  }
  
  # FE columns (no expansion needed - used for grouping)
  fe_sql = if (length(inputs$fe)) paste(inputs$fe, collapse = ", ") else NULL
  
  # Combined columns for SELECT and GROUP BY
  all_cols_sql = if (!is.null(fe_sql)) paste(xvars_sql, fe_sql, sep = ", ") else xvars_sql
  group_cols = if (!is.null(fe_sql)) c(xvar_names, inputs$fe) else xvar_names
  group_cols_sql = paste(group_cols, collapse = ", ")
  
  query_string = paste0(
    "WITH cte AS (
    SELECT
        ",
    all_cols_sql,
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

  if (inputs$sql_only) {
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

  if (length(inputs$fe)) {
    for (f in inputs$fe) {
      compressed_dat[[f]] = factor(compressed_dat[[f]])
    }
  }
  if (inputs$data_only) {
    return(compressed_dat)
  }

  # Build design matrix
  # Use expanded column names if interactions were present
  design_vars = if (isTRUE(inputs$has_interactions)) xvar_names else inputs$xvars
  X = sparse.model.matrix(
    reformulate(c(design_vars, inputs$fe)),
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
  
  # For clustered SEs, need to query cluster-by-cell stats
  meat = NULL
  n_params_cluster = ncol(X)  # K for CR1 correction
  if (inputs$vcov_type_req == "cluster") {
    meat = compute_meat_cluster_compress(
      conn = inputs$conn,
      from_statement = from_statement,
      group_cols = group_cols,
      yvar = inputs$yvar,
      cluster_var = inputs$cluster_var,
      compressed_dat = compressed_dat,
      X = X,
      yhat = yhat
    )
    # For ssc = "nested", exclude nested FE levels from K
    if (inputs$ssc == "nested") {
      nested_levels = count_nested_fe_levels(
        inputs$conn, from_statement, inputs$fe, inputs$cluster_var
      )
      n_params_cluster = ncol(X) - nested_levels
    }
  }
  
  vcov_mat = compute_vcov(
    vcov_type = inputs$vcov_type_req,
    strategy = "compress",
    XtX_inv = XtX_inv,
    rss = rss_total,
    df_res = df_res,
    nobs_orig = nobs_orig,
    n_params = n_params_cluster,
    X = X,
    rss_g = rss_g,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss_total
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, max(nobs_orig - ncol(X), 1))

  # Coefficient names for xvars (excluding intercept and FE dummies)
  # Match coefficients that start with any design_var name
  all_coef_names = rownames(coeftable)
  all_coef_names = all_coef_names[all_coef_names != "(Intercept)"]
  design_pattern = paste0("^(", paste(design_vars, collapse = "|"), ")")
  coef_names = all_coef_names[grepl(design_pattern, all_coef_names)]

  return(
    list(
      coeftable = coeftable,
      data = compressed_dat,
      vcov = vcov_mat,
      fml = inputs$fml,
      yvar = inputs$yvar,
      xvars = inputs$xvars,
      coef_names = coef_names,
      fe = inputs$fe,
      query_string = query_string,
      nobs = nobs_comp,
      nobs_orig = nobs_orig,
      strategy = "compress",
      compression_ratio = compression_ratio,
      compression_ratio_est = inputs$compression_ratio_est,
      df_residual = max(nobs_orig - ncol(X), 1)
    )
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

#' Count levels of FE variables nested within cluster variable
#' 
#' For ssc = "nested", we exclude FE levels from K if the FE is nested within
#' the cluster variable (i.e., each FE value belongs to exactly one cluster).
#' 
#' @keywords internal
count_nested_fe_levels = function(conn, from_statement, fe, cluster_var) {
  if (is.null(fe) || length(fe) == 0 || is.null(cluster_var)) {
    return(0L)
  }
  
  nested_levels = 0L
  for (f in fe) {
    # Check if FE is nested: each FE value should map to exactly one cluster
    # If any FE value spans multiple clusters, it's not nested
    nested_sql = glue(
      "SELECT 1 FROM (SELECT * {from_statement}) t ",
      "GROUP BY {f} ",
      "HAVING COUNT(DISTINCT {cluster_var}) > 1 ",
      "LIMIT 1"
    )
    result = tryCatch(dbGetQuery(conn, nested_sql), error = function(e) NULL)
    
    if (is.null(result) || nrow(result) == 0) {
      # FE is nested; count its levels
      count_sql = glue(
        "SELECT COUNT(DISTINCT {f}) AS n FROM (SELECT * {from_statement}) t"
      )
      n_levels = tryCatch(dbGetQuery(conn, count_sql)$n, error = function(e) 0L)
      nested_levels = nested_levels + n_levels
    }
  }
  
  nested_levels
}

#' Compute variance-covariance matrix
#' @keywords internal
compute_vcov = function(
  vcov_type = "iid",
  strategy = "compress",
  XtX_inv,
  rss,
  df_res,
  nobs_orig, # N
  n_params = NULL, # K (for CR1 correction)
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
  } else if (vcov_type == "cluster") {
    # Cluster-robust (CR1) standard errors
    if (is.null(meat)) {
      stop("Clustered SEs require meat matrix from compute_meat_cluster_sql")
    }
    n_clusters = attr(meat, "n_clusters") # G
    if (is.null(n_clusters)) {
      stop("Meat matrix missing n_clusters attribute")
    }
    if (is.null(n_params)) n_params = ncol(XtX_inv)
    # CR1 small-sample correction: (G/(G-1)) * (N/(N-K))
    scale_cr1 = (n_clusters / (n_clusters - 1)) * (nobs_orig / (nobs_orig - n_params))
    vcov_mat = scale_cr1 * (XtX_inv %*% meat %*% XtX_inv)
    attr(vcov_mat, "type") = "cluster"
    attr(vcov_mat, "n_clusters") = n_clusters
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
                            has_intercept = FALSE,
                            vars_sql = NULL) {
  # Build variable SQL expressions (use provided or construct from suffix)
  if (is.null(vars_sql)) {
    vars_sql = paste0(vars, var_suffix)
  }
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
  
  # Build meat terms using numeric indices to avoid naming collisions
  # (variable names may contain underscores from interaction terms)
  meat_terms = character(0)
  
  if (has_intercept) {
    meat_terms = c(meat_terms, sprintf(
      "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS meat_0_0",
      resid_expr, resid_expr
    ))
    for (j in seq_along(vars)) {
      meat_terms = c(meat_terms, sprintf(
        "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS meat_0_%d",
        resid_expr, resid_expr, vars_sql[j], j
      ))
    }
  }
  
  for (i in seq_along(vars)) {
    for (j in i:length(vars)) {
      meat_terms = c(meat_terms, sprintf(
        "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT) * CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS meat_%d_%d",
        resid_expr, resid_expr, vars_sql[i], vars_sql[j], i, j
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
    meat_mat[1, 1] = meat_df$meat_0_0
    for (j in seq_along(vars)) {
      val = meat_df[[sprintf("meat_0_%d", j)]]
      meat_mat[1, j + 1] = meat_mat[j + 1, 1] = val
    }
  }
  
  for (i in seq_along(vars)) {
    for (j in i:length(vars)) {
      val = meat_df[[sprintf("meat_%d_%d", i, j)]]
      idx_i = if (has_intercept) i + 1 else i
      idx_j = if (has_intercept) j + 1 else j
      meat_mat[idx_i, idx_j] = meat_mat[idx_j, idx_i] = val
    }
  }
  meat_mat
}

#' Compute cluster-robust meat matrix via SQL
#' 
#' Computes the meat matrix for cluster-robust (CR0) standard errors by
#' aggregating score vectors within clusters and computing outer products.
#' 
#' @keywords internal
compute_meat_cluster_sql = function(conn, cte_sql, vars, yvar, betahat, 
                                    cluster_var,
                                    is_athena = FALSE, 
                                    var_suffix = "_tilde",
                                    cte_name = "demeaned",
                                    has_intercept = FALSE,
                                    vars_sql = NULL) {
  # Build variable SQL expressions (use provided or construct from suffix)
  if (is.null(vars_sql)) {
    vars_sql = paste0(vars, var_suffix)
  }
  yvar_sql = paste0(yvar, var_suffix)
  
  # Extract beta values
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
  
  # Build cluster score terms using numeric indices to avoid naming collisions
  score_terms = character(0)
  
  if (has_intercept) {
    score_terms = c(score_terms, sprintf(
      "SUM(CAST(%s AS FLOAT)) AS score_0",
      resid_expr
    ))
  }
  
  for (j in seq_along(vars)) {
    score_terms = c(score_terms, sprintf(
      "SUM(CAST(%s AS FLOAT) * CAST(%s AS FLOAT)) AS score_%d",
      resid_expr, vars_sql[j], j
    ))
  }
  
  # Query: aggregate scores by cluster
  cluster_sql = paste0(
    cte_sql,
    ",\ncluster_scores AS (\n  SELECT ", cluster_var, ", ",
    paste(score_terms, collapse = ", "),
    "\n  FROM ", cte_name,
    "\n  GROUP BY ", cluster_var,
    "\n)\nSELECT * FROM cluster_scores"
  )
  
  if (is_athena) {
    cluster_sql = gsub("FLOAT", "REAL", cluster_sql, fixed = TRUE)
  }
  
  cluster_df = dbGetQuery(conn, cluster_sql)
  
  # Build meat matrix from cluster scores: M = sum_g(s_g %*% t(s_g))
  if (has_intercept) {
    vars_all = c("(Intercept)", vars)
  } else {
    vars_all = vars
  }
  p = length(vars_all)
  meat_mat = matrix(0, p, p, dimnames = list(vars_all, vars_all))
  
  # Extract score columns
  score_cols = if (has_intercept) {
    c("score_0", paste0("score_", seq_along(vars)))
  } else {
    paste0("score_", seq_along(vars))
  }
  
  # Sum outer products across clusters
  for (i in seq_len(nrow(cluster_df))) {
    s_g = as.numeric(cluster_df[i, score_cols])
    meat_mat = meat_mat + tcrossprod(s_g)
  }
  
  # Return meat matrix and number of clusters for df adjustment
  attr(meat_mat, "n_clusters") = nrow(cluster_df)
  meat_mat
}

#' Compute cluster-robust meat matrix for compress strategy
#' 
#' For compress strategy, we need to query cluster-by-cell stats from the
#' original data, then compute cluster scores using the cell-level fitted values.
#' 
#' @keywords internal
compute_meat_cluster_compress = function(conn, from_statement, group_cols, 
                                         yvar, cluster_var, compressed_dat,
                                         X, yhat) {
  group_cols_sql = paste(group_cols, collapse = ", ")
  
  # Query cluster-by-cell sufficient statistics
  cluster_cell_sql = paste0(
    "SELECT ", cluster_var, ", ", group_cols_sql, ",\n",
    "  COUNT(*) AS n_gc,\n",
    "  SUM(", yvar, ") AS sum_y_gc\n",
    from_statement, "\n",
    "GROUP BY ", cluster_var, ", ", group_cols_sql
  )
  
  cluster_cell_df = dbGetQuery(conn, cluster_cell_sql)
  
  # Create cell key for matching (same grouping as compress strategy)
  compressed_dat$cell_key = interaction(compressed_dat[, group_cols, drop = FALSE])
  cluster_cell_df$cell_key = interaction(cluster_cell_df[, group_cols, drop = FALSE])
  
  # Add yhat to compressed_dat and create lookup
  compressed_dat$yhat = yhat
  yhat_lookup = compressed_dat[, c("cell_key", "yhat")]
  
  # Merge to get yhat for each cluster-cell combo (only keep needed cols from cluster_cell_df)
  cluster_cell_df = merge(
    cluster_cell_df[, c("cell_key", cluster_var, "n_gc", "sum_y_gc")], 
    yhat_lookup, 
    by = "cell_key", 
    all.x = TRUE
  )
  
  # Compute summed residuals per (cluster, cell): u_sum_gc = sum_y_gc - n_gc * yhat
  cluster_cell_df$u_sum_gc = cluster_cell_df$sum_y_gc - cluster_cell_df$n_gc * cluster_cell_df$yhat
  
  # Get unique clusters
  clusters = unique(cluster_cell_df[[cluster_var]])
  n_clusters = length(clusters)
  p = ncol(X)
  
  # Initialize meat matrix
  meat_mat = matrix(0, p, p, dimnames = list(colnames(X), colnames(X)))
  
  # Create cell_key to row index mapping for X matrix
  cell_to_row = setNames(seq_len(nrow(compressed_dat)), as.character(compressed_dat$cell_key))
  
  # For each cluster, compute score vector and add outer product to meat
  for (g in clusters) {
    cells_in_g = cluster_cell_df[cluster_cell_df[[cluster_var]] == g, ]
    
    # Find which rows in X correspond to these cells
    cell_matches = cell_to_row[as.character(cells_in_g$cell_key)]
    
    # Compute s_g = X' * u_sum (weighted by u_sum_gc for each cell)
    s_g = as.numeric(crossprod(X[cell_matches, , drop = FALSE], cells_in_g$u_sum_gc))
    meat_mat = meat_mat + tcrossprod(s_g)
  }
  
  attr(meat_mat, "n_clusters") = n_clusters
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
  if (inputs$sql_only) {
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
