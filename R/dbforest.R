#' Run a random forest regression on a database backend
#'
#' @md
#' @description
#' Fits an ensemble of regression trees using bootstrap resampling and random
#' feature subsampling. Each tree is trained with \code{\link{dbtree}} on a
#' weighted bootstrap sample, and predictions are averaged across trees.
#'
#' The \code{sample_frac} argument defines a fixed global sampling pool of
#' size \code{ceiling(sample_frac * N)} determined by \code{seed}. Each tree
#' then draws a bootstrap sample (with replacement) from this pool.
#'
#' @param fml A \code{\link[stats]{formula}} for the regression tree model.
#'   Interactions and fixed effects (\code{|}) are not supported.
#' @inheritParams dbtree
#' @param ntree Number of trees in the forest. Default is 200.
#' @param mtry Number of predictors randomly considered at each split. If
#'   \code{NULL}, uses \code{max(1, floor(p / 3))} where \code{p} is the number
#'   of predictors.
#' @param sample_frac Fraction in \code{(0, 1]} controlling the size of the
#'   fixed sampling pool used by all trees. Default is 1.
#' @param seed Integer random seed controlling pool construction, bootstrap
#'   draws, and per-tree split randomization.
#'
#' @return An object of class \code{"dbforest"}.
#' @export
dbforest = function(
  fml,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  weights = NULL,
  ntree = 200,
  mtry = NULL,
  sample_frac = 1,
  seed = 42,
  bin_scope = c("global", "node"),
  n_bins = 128,
  bin_method = c("quantile", "width"),
  bin_sample_frac = NULL,
  max_depth = 5,
  min_split = 20,
  min_leaf = 5,
  min_gain = 0,
  max_cat = 50,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  dots = list(...)
  if ("replace" %in% names(dots)) {
    stop("`replace` is no longer supported. dbforest always bootstraps with replacement.")
  }

  verbose = isTRUE(verbose)
  ntree = as.integer(ntree)
  if (!is.numeric(ntree) || length(ntree) != 1 || is.na(ntree) || ntree < 1) {
    stop("ntree must be an integer >= 1.")
  }
  if (!is.numeric(sample_frac) || length(sample_frac) != 1 || is.na(sample_frac) || sample_frac <= 0 || sample_frac > 1) {
    stop("sample_frac must be a numeric scalar in (0, 1].")
  }
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("seed must be a numeric scalar.")
  }
  seed = as.integer(seed)
  bin_scope = match.arg(bin_scope)
  bin_method = match.arg(bin_method)

  fml_parsed = parse_regression_formula(fml)
  if (fml_parsed$has_interactions) {
    stop("dbforest does not support interaction terms.")
  }
  if (!is.null(fml_parsed$fe)) {
    stop("dbforest does not support fixed effects (terms after |).")
  }
  fml = fml_parsed$fml
  yvar = fml_parsed$yvar
  xvars = unique(fml_parsed$xvars)
  p = length(xvars)
  if (is.null(mtry)) {
    mtry = max(1L, floor(p / 3))
  }
  if (!is.numeric(mtry) || length(mtry) != 1 || is.na(mtry)) {
    stop("mtry must be NULL or a numeric scalar.")
  }
  mtry = as.integer(mtry)
  if (mtry < 1 || mtry > p) {
    stop(sprintf("mtry must be in [1, %d].", p))
  }

  db_setup = setup_db_connection(conn, table, data, path, caller = "dbforest")
  conn = db_setup$conn
  conn_managed = db_setup$own_conn
  from_statement = db_setup$from_statement
  registered_table = db_setup$registered_table
  backend = detect_backend(conn)$name

  cleanup_conn = function() {
    if (conn_managed && dbIsValid(conn)) {
      dbDisconnect(conn, shutdown = TRUE)
    }
  }
  on.exit(cleanup_conn(), add = TRUE)

  cleanup_registered = function() {
    if (!is.null(registered_table) && dbIsValid(conn)) {
      try(duckdb_unregister(conn, registered_table), silent = TRUE)
    }
  }
  on.exit(cleanup_registered(), add = TRUE)

  tmp_stamp = gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%S%OS3"))
  base_table = dbreg_temp_table_name(paste0("__dbforest_base_", tmp_stamp), backend)
  pool_table = dbreg_temp_table_name(paste0("__dbforest_pool_", tmp_stamp), backend)
  temp_tables = c(base_table, pool_table)

  cleanup_temp_tables = function() {
    for (tbl in unique(temp_tables)) {
      dbreg_drop_table(conn, tbl, backend)
    }
  }
  on.exit(cleanup_temp_tables(), add = TRUE)

  if (!is.null(weights)) {
    if (!is.character(weights) || length(weights) != 1) {
      stop("weights must be a single character string naming a column.")
    }
    neg_sql = glue("SELECT {sql_count_expr(backend)} AS n {from_statement} WHERE {weights} < 0")
    neg_count = tryCatch(dbGetQuery(conn, neg_sql)$n, error = function(e) 0)
    if (neg_count > 0) {
      stop("Weights must be non-negative. Found negative values.")
    }
  }

  where_clause = dbtree_build_where_clause(yvar, xvars, weights)
  base_sql = glue("
    SELECT
      ROW_NUMBER() OVER (ORDER BY 1) AS __rf_rowid,
      *
    {from_statement}
    WHERE {where_clause}
  ")
  dbreg_create_temp_table_as(conn, base_table, base_sql, backend)

  n_sql = glue("SELECT {sql_count_expr(backend)} AS n FROM {base_table}")
  nobs = as.numeric(dbGetQuery(conn, n_sql)$n[1])
  if (!is.finite(nobs) || nobs <= 1) {
    stop("Not enough non-missing rows to fit a forest.")
  }
  pool_n = as.integer(ceiling(sample_frac * nobs))
  pool_n = max(1L, min(pool_n, as.integer(nobs)))

  pool_key_expr = sprintf("ABS(SIN((__rf_rowid + %.10f) * 12.9898))", as.numeric(seed))
  pool_rank_sql = glue("
    SELECT
      ROW_NUMBER() OVER (ORDER BY __rf_key, __rf_rowid) AS __rf_pool_id,
      ranked.*
    FROM (
      SELECT
        b.*,
        {pool_key_expr} AS __rf_key
      FROM {base_table} b
    ) ranked
    ORDER BY __rf_key, __rf_rowid
  ")
  pool_sql = sql_limit(pool_rank_sql, pool_n, backend)
  dbreg_create_temp_table_as(conn, pool_table, pool_sql, backend)

  if (verbose) {
    message(sprintf(
      "[dbforest] Fitting %d trees (pool=%d/%d, mtry=%d, bootstrap=TRUE)",
      ntree, pool_n, as.integer(nobs), mtry
    ))
  }

  trees = vector("list", ntree)
  tree_seed0 = as.integer(seed)
  for (b in seq_len(ntree)) {
    tree_table = dbreg_temp_table_name(paste0("__dbforest_tree_", tmp_stamp, "_", b), backend)
    temp_tables = c(temp_tables, tree_table)
    tree_weight_expr = if (is.null(weights)) {
      "1.0"
    } else {
      paste0("1.0 * p.", weights)
    }

    draw_n = pool_n
    draw_index_sql = sql_limit(
      glue("SELECT ROW_NUMBER() OVER (ORDER BY __rf_rowid) AS j FROM {base_table}"),
      draw_n,
      backend
    )
    seed_offset = as.integer(tree_seed0 + b * 10007L)
    draw_expr = glue("
      1 + CAST(FLOOR(
        {pool_n} * CASE
          WHEN ABS(SIN((j + {seed_offset}) * 12.9898)) >= 0.999999999999
            THEN 0.999999999999
          ELSE ABS(SIN((j + {seed_offset}) * 12.9898))
        END
      ) AS BIGINT)
    ")

    tree_sql = paste0(
      "WITH draw_index AS (", draw_index_sql, "),\n",
      "draws AS (\n",
      "  SELECT ", draw_expr, " AS __rf_pool_id\n",
      "  FROM draw_index\n",
      "),\n",
      "counts AS (\n",
      "  SELECT __rf_pool_id, ", sql_count_expr(backend), " AS __rf_boot_w\n",
      "  FROM draws\n",
      "  GROUP BY __rf_pool_id\n",
      ")\n",
      "SELECT p.*, (1.0 * c.__rf_boot_w) * (", tree_weight_expr, ") AS __rf_tree_w\n",
      "FROM ", pool_table, " p\n",
      "JOIN counts c ON p.__rf_pool_id = c.__rf_pool_id"
    )

    dbreg_create_temp_table_as(conn, tree_table, tree_sql, backend)

    trees[[b]] = dbtree(
      fml = fml,
      conn = conn,
      table = tree_table,
      weights = "__rf_tree_w",
      bin_scope = bin_scope,
      n_bins = n_bins,
      bin_method = bin_method,
      bin_sample_frac = bin_sample_frac,
      max_depth = max_depth,
      min_split = min_split,
      min_leaf = min_leaf,
      min_gain = min_gain,
      max_cat = max_cat,
      mtry = mtry,
      seed = as.integer(tree_seed0 + b),
      drop_missings = TRUE,
      verbose = FALSE
    )

    dbreg_drop_table(conn, tree_table, backend)
    if (verbose && (b %% max(1L, floor(ntree / 10)) == 0L || b == ntree)) {
      message(sprintf("[dbforest] Fitted %d/%d trees", b, ntree))
    }
  }

  out = list(
    call = match.call(),
    fml = fml,
    yvar = yvar,
    xvars = xvars,
    trees = trees,
    ntree = ntree,
    mtry = mtry,
    sample_frac = sample_frac,
    pool_n = pool_n,
    seed = seed,
    nobs = as.integer(nobs),
    backend = backend,
    params = list(
      bin_scope = bin_scope,
      n_bins = n_bins,
      bin_method = bin_method,
      bin_sample_frac = bin_sample_frac,
      max_depth = max_depth,
      min_split = min_split,
      min_leaf = min_leaf,
      min_gain = min_gain,
      max_cat = max_cat
    )
  )
  class(out) = "dbforest"
  out
}
