#' Run a regression tree on a database backend
#'
#' @md
#' @description
#' Fits an approximate regression tree using SQL aggregation on a database backend.
#' Numeric splits are evaluated on binned candidates (quantiles or equal-width),
#' while categorical splits are evaluated as one-vs-rest. The heavy lifting
#' (binning and sufficient statistics) happens in the database; only aggregated
#' results are returned to R.
#'
#' @param fml A \code{\link[stats]{formula}} representing the relation to be estimated.
#'   Interactions are not supported. Fixed effects after \code{|} are not supported.
#' @inheritParams dbreg
#' @param weights Character string specifying the column name to use as weights,
#'   or \code{NULL} (default) for unweighted trees. Weights must be non-negative;
#'   rows with zero weight are dropped.
#' @param bin_scope One of \code{"global"} or \code{"node"}. With \code{"global"},
#'   numeric bins are computed once for the full dataset and reused at all nodes.
#'   With \code{"node"}, numeric bins are computed within each node at each depth.
#' @param n_bins Integer number of bins for numeric split candidates. Default is 128.
#' @param bin_method Binning method for numeric features. One of \code{"quantile"}
#'   (default) or \code{"width"}.
#' @param bin_sample_frac Optional fraction in (0,1] used to compute global bins.
#'   If \code{NULL} (default), sampling is automatic: 1% for datasets exceeding
#'   1 million rows and 100% otherwise.
#' @param max_depth Maximum tree depth (root is depth 0). Default is 5.
#' @param min_split Minimum number of rows required to split a node. Default is 20.
#' @param min_leaf Minimum number of rows required in each leaf. Default is 5.
#' @param min_gain Minimum SSE reduction required to accept a split. Default is 0.
#' @param max_cat Maximum distinct categories allowed for categorical predictors.
#'   Default is 50.
#' @param mtry Optional integer giving the number of predictors randomly
#'   considered at each split. If \code{NULL} (default), all predictors are
#'   considered.
#' @param seed Optional integer random seed used when \code{mtry} is smaller
#'   than the total number of predictors.
#' @param verbose Logical. Print progress messages? Defaults to \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of class \code{"dbtree"} containing:
#' \describe{
#'   \item{nodes}{Data frame describing the fitted tree.}
#'   \item{metrics}{List with training RMSE and R-squared.}
#'   \item{params}{List of tuning parameters.}
#' }
#'
#' @export
#' @examples
#' # Simple in-memory example
#' df = data.frame(y = c(1, 1, 1, 10, 10, 10), x = c(0, 0, 0, 1, 1, 1))
#' mod = dbtree(y ~ x, data = df, max_depth = 1, n_bins = 2)
#' predict(mod, df)
dbtree = function(
  fml,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  weights = NULL,
  bin_scope = c("global", "node"),
  n_bins = 128,
  bin_method = c("quantile", "width"),
  bin_sample_frac = NULL,
  max_depth = 5,
  min_split = 20,
  min_leaf = 5,
  min_gain = 0,
  max_cat = 50,
  mtry = NULL,
  seed = NULL,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  verbose = isTRUE(verbose)
  bin_scope = match.arg(bin_scope)
  bin_method = match.arg(bin_method)
  n_bins = as.integer(n_bins)
  max_depth = as.integer(max_depth)
  min_split = as.integer(min_split)
  min_leaf = as.integer(min_leaf)
  max_cat = as.integer(max_cat)
  
  if (!is.numeric(n_bins) || n_bins < 2) stop("n_bins must be an integer >= 2.")
  if (!is.numeric(max_depth) || max_depth < 0) stop("max_depth must be >= 0.")
  if (!is.numeric(min_split) || min_split < 2) stop("min_split must be >= 2.")
  if (!is.numeric(min_leaf) || min_leaf < 1) stop("min_leaf must be >= 1.")
  if (!is.numeric(min_gain) || min_gain < 0) stop("min_gain must be >= 0.")
  if (!is.numeric(max_cat) || max_cat < 2) stop("max_cat must be >= 2.")
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed))) {
    stop("seed must be NULL or a numeric scalar.")
  }
  if (!is.null(seed)) {
    seed = as.integer(seed)
  }
  
  if (!isTRUE(drop_missings)) {
    warning("dbtree currently drops rows with missing values. drop_missings = FALSE is ignored.")
  }
  drop_missings = TRUE
  
  # Parse formula
  fml_parsed = parse_regression_formula(fml)
  if (fml_parsed$has_interactions) {
    stop("dbtree does not support interaction terms (e.g., x * z or x:z). ",
         "Please specify controls as separate additive terms (e.g., y ~ x + z).")
  }
  if (!is.null(fml_parsed$fe)) {
    stop("dbtree does not support fixed effects (terms after |).")
  }
  fml = fml_parsed$fml
  yvar = fml_parsed$yvar
  xvars = unique(fml_parsed$xvars)
  p = length(xvars)
  if (is.null(mtry)) {
    mtry = p
  }
  if (!is.numeric(mtry) || length(mtry) != 1 || is.na(mtry)) {
    stop("mtry must be NULL or a numeric scalar.")
  }
  mtry = as.integer(mtry)
  if (mtry < 1 || mtry > p) {
    stop(sprintf("mtry must be in [1, %d].", p))
  }

  # Set up database connection and data source
  db_setup = setup_db_connection(conn, table, data, path, caller = "dbtree")
  conn = db_setup$conn
  conn_managed = db_setup$own_conn
  table_name = db_setup$table_name
  from_statement = db_setup$from_statement
  registered_table = db_setup$registered_table
  
  # Backend info
  backend_info = detect_backend(conn)
  backend = backend_info$name
  
  # Cleanup
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
  
  # Validate weights
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
  
  # Build WHERE clause for missing values and weights
  where_clause = dbtree_build_where_clause(yvar, xvars, weights)
  
  # Row count
  n_rows_sql = glue("SELECT {sql_count_expr(backend)} AS n {from_statement} WHERE {where_clause}")
  n_rows = as.numeric(dbGetQuery(conn, n_rows_sql)$n)
  if (!is.finite(n_rows) || n_rows <= 1) {
    stop("Not enough non-missing rows to fit a tree.")
  }
  
  # Detect variable types
  col_info = get_column_info(conn, table_name, xvars)
  var_types = col_info$types
  numeric_vars = names(var_types)[var_types == "numeric"]
  cat_vars = names(var_types)[var_types == "factor"]
  
  # Categorical cardinality checks
  if (length(cat_vars) > 0) {
    for (v in cat_vars) {
      card_sql = glue("SELECT COUNT(DISTINCT {v}) AS n {from_statement} WHERE {where_clause}")
      n_levels = as.numeric(dbGetQuery(conn, card_sql)$n)
      if (n_levels > max_cat) {
        stop(sprintf(
          "Categorical variable '%s' has %d levels (max_cat = %d). ",
          v, n_levels, max_cat
        ), "Increase max_cat or remove this variable.")
      }
    }
  }
  
  # Compute global bins for numeric variables (if requested)
  global_bins = list()
  if (bin_scope == "global" && length(numeric_vars) > 0) {
    if (verbose) message("[dbtree] Computing global bins for numeric features...")
    for (v in numeric_vars) {
      global_bins[[v]] = dbtree_compute_global_bins(
        conn = conn,
        table_name = table_name,
        from_statement = from_statement,
        xvar = v,
        n_bins = n_bins,
        bin_method = bin_method,
        bin_sample_frac = bin_sample_frac,
        backend = backend,
        where_clause = where_clause,
        n_rows = n_rows,
        verbose = verbose
      )
    }
  }
  
  # Root stats
  w_expr = if (is.null(weights)) "1.0" else weights
  root_sql = glue("
    SELECT
      {sql_count_expr(backend)} AS n,
      SUM({w_expr}) AS sum_w,
      SUM({w_expr} * {yvar}) AS sum_wy,
      SUM({w_expr} * {yvar} * {yvar}) AS sum_wy2
    {from_statement}
    WHERE {where_clause}
  ")
  root_stats = dbGetQuery(conn, root_sql)
  root_n = as.numeric(root_stats$n)
  root_sum_w = as.numeric(root_stats$sum_w)
  root_sum_wy = as.numeric(root_stats$sum_wy)
  root_sum_wy2 = as.numeric(root_stats$sum_wy2)
  
  # Initialize tree
  nodes = data.frame(
    node_id = 1L,
    depth = 0L,
    n = root_n,
    sum_w = root_sum_w,
    sum_wy = root_sum_wy,
    sum_wy2 = root_sum_wy2,
    prediction = root_sum_wy / root_sum_w,
    is_leaf = TRUE,
    split_var = NA_character_,
    split_type = NA_character_,
    split_value = NA_character_,
    left_id = NA_integer_,
    right_id = NA_integer_,
    gain = NA_real_,
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
  node_conditions = list("1" = character())
  next_node_id = 2L
  
  rng_state = NULL
  if (!is.null(seed)) {
    rng_state = if (exists(".Random.seed", envir = .GlobalEnv)) .Random.seed else NULL
    set.seed(seed)
    on.exit({
      if (is.null(rng_state)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        .Random.seed <<- rng_state
      }
    }, add = TRUE)
  }
  
  # Main training loop
  if (max_depth > 0) {
    for (depth in 0:(max_depth - 1)) {
      split_candidates = nodes$node_id[nodes$is_leaf & nodes$depth == depth & nodes$n >= min_split]
      if (!length(split_candidates)) next
      
      if (verbose) {
        message(sprintf("[dbtree] Depth %d: evaluating %d node(s)...", depth, length(split_candidates)))
      }
      
      node_case = dbtree_build_node_case(node_conditions, split_candidates)
      
      # Numeric features: bin stats per node
      numeric_stats = list()
      if (length(numeric_vars) > 0) {
        for (v in numeric_vars) {
          numeric_stats[[v]] = dbtree_query_numeric_bins(
            conn = conn,
            from_statement = from_statement,
            node_case = node_case,
            yvar = yvar,
            xvar = v,
            w_expr = w_expr,
            where_clause = where_clause,
            bin_scope = bin_scope,
            n_bins = n_bins,
            bin_method = bin_method,
            global_bounds = global_bins[[v]],
            backend = backend
          )
        }
      }
      
      # Categorical features: level stats per node
      cat_stats = list()
      if (length(cat_vars) > 0) {
        for (v in cat_vars) {
          cat_stats[[v]] = dbtree_query_categorical_stats(
            conn = conn,
            from_statement = from_statement,
            node_case = node_case,
            yvar = yvar,
            xvar = v,
            w_expr = w_expr,
            where_clause = where_clause
          )
        }
      }
      
      # Evaluate splits for each candidate node
      for (node_id in split_candidates) {
        node_row = nodes[nodes$node_id == node_id, , drop = FALSE]
        total_stats = list(
          n = node_row$n,
          sum_w = node_row$sum_w,
          sum_wy = node_row$sum_wy,
          sum_wy2 = node_row$sum_wy2
        )
        parent_sse = dbtree_sse(total_stats$sum_w, total_stats$sum_wy, total_stats$sum_wy2)
        
        best = list(gain = -Inf)
        split_vars = xvars
        if (mtry < length(xvars)) {
          split_vars = sample(xvars, size = mtry, replace = FALSE)
        }

        # Numeric splits
        if (length(numeric_stats) > 0) {
          for (v in intersect(names(numeric_stats), split_vars)) {
            stats_v = numeric_stats[[v]]
            stats_node = stats_v[stats_v$node_id == node_id, , drop = FALSE]
            if (nrow(stats_node) < 2) next
            scored = dbtree_score_numeric(stats_node, total_stats, parent_sse, min_leaf, min_gain)
            if (!is.null(scored) && scored$gain > best$gain) {
              best = c(scored, list(split_var = v, split_type = "numeric"))
            }
          }
        }
        
        # Categorical splits (one-vs-rest)
        if (length(cat_stats) > 0) {
          for (v in intersect(names(cat_stats), split_vars)) {
            stats_v = cat_stats[[v]]
            stats_node = stats_v[stats_v$node_id == node_id, , drop = FALSE]
            if (nrow(stats_node) < 2) next
            scored = dbtree_score_categorical(stats_node, total_stats, parent_sse, min_leaf, min_gain)
            if (!is.null(scored) && scored$gain > best$gain) {
              best = c(scored, list(split_var = v, split_type = "categorical"))
            }
          }
        }
        
        # Apply best split if valid
        if (is.finite(best$gain) && best$gain >= min_gain) {
          left_id = next_node_id
          right_id = next_node_id + 1L
          next_node_id = next_node_id + 2L
          
          # Update parent
          nodes$split_var[nodes$node_id == node_id] = best$split_var
          nodes$split_type[nodes$node_id == node_id] = best$split_type
          nodes$split_value[nodes$node_id == node_id] = best$split_value
          nodes$left_id[nodes$node_id == node_id] = left_id
          nodes$right_id[nodes$node_id == node_id] = right_id
          nodes$gain[nodes$node_id == node_id] = best$gain
          nodes$is_leaf[nodes$node_id == node_id] = FALSE
          
          # Add children
          left_pred = best$left$sum_wy / best$left$sum_w
          right_pred = best$right$sum_wy / best$right$sum_w
          
          nodes = rbind(
            nodes,
            data.frame(
              node_id = left_id,
              depth = depth + 1L,
              n = best$left$n,
              sum_w = best$left$sum_w,
              sum_wy = best$left$sum_wy,
              sum_wy2 = best$left$sum_wy2,
              prediction = left_pred,
              is_leaf = TRUE,
              split_var = NA_character_,
              split_type = NA_character_,
              split_value = NA_character_,
              left_id = NA_integer_,
              right_id = NA_integer_,
              gain = NA_real_,
              reason = NA_character_,
              stringsAsFactors = FALSE
            ),
            data.frame(
              node_id = right_id,
              depth = depth + 1L,
              n = best$right$n,
              sum_w = best$right$sum_w,
              sum_wy = best$right$sum_wy,
              sum_wy2 = best$right$sum_wy2,
              prediction = right_pred,
              is_leaf = TRUE,
              split_var = NA_character_,
              split_type = NA_character_,
              split_value = NA_character_,
              left_id = NA_integer_,
              right_id = NA_integer_,
              gain = NA_real_,
              reason = NA_character_,
              stringsAsFactors = FALSE
            )
          )
          
          # Update node conditions
          parent_conds = node_conditions[[as.character(node_id)]]
          if (best$split_type == "numeric") {
            cond_left = paste0(best$split_var, " <= ", best$split_value)
            cond_right = paste0(best$split_var, " > ", best$split_value)
          } else {
            val_escaped = gsub("'", "''", best$split_value, fixed = TRUE)
            cond_left = paste0(best$split_var, " = '", val_escaped, "'")
            cond_right = paste0(best$split_var, " <> '", val_escaped, "'")
          }
          node_conditions[[as.character(left_id)]] = c(parent_conds, cond_left)
          node_conditions[[as.character(right_id)]] = c(parent_conds, cond_right)
        } else {
          # If the node cannot possibly satisfy min_leaf on both children,
          # record that explicit stopping cause instead of generic no_split.
          if (node_row$n < (2L * min_leaf)) {
            nodes$reason[nodes$node_id == node_id] = "min_leaf"
          } else {
            nodes$reason[nodes$node_id == node_id] = "no_split"
          }
        }
      }
    }
  }
  
  # Mark max_depth leaves
  nodes$reason[nodes$is_leaf & nodes$depth >= max_depth & is.na(nodes$reason)] = "max_depth"
  nodes$reason[nodes$is_leaf & nodes$n < min_split & is.na(nodes$reason)] = "min_split"
  
  # Training metrics
  leaf_nodes = nodes[nodes$is_leaf, , drop = FALSE]
  rss = sum(dbtree_sse(leaf_nodes$sum_w, leaf_nodes$sum_wy, leaf_nodes$sum_wy2))
  tss = dbtree_sse(root_sum_w, root_sum_wy, root_sum_wy2)
  rmse_denom = if (is.null(weights)) root_n else root_sum_w
  rmse = sqrt(rss / rmse_denom)
  r2 = 1 - rss / tss
  
  out = list(
    call = match.call(),
    fml = fml,
    yvar = yvar,
    xvars = xvars,
    weights = weights,
    nodes = nodes,
    nobs = root_n,
    metrics = list(rmse = rmse, r2 = r2, rss = rss, tss = tss),
    params = list(
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
      seed = seed
    ),
    backend = backend
  )
  class(out) = "dbtree"
  out
}

# =============================================================================
# Internal helpers
# =============================================================================

dbtree_build_where_clause = function(yvar, xvars, weights = NULL) {
  terms = c(
    sprintf("%s IS NOT NULL", yvar),
    sprintf("%s IS NOT NULL", xvars)
  )
  if (!is.null(weights)) {
    terms = c(terms, sprintf("%s IS NOT NULL", weights), sprintf("%s > 0", weights))
  }
  paste(terms, collapse = " AND ")
}

dbtree_build_node_case = function(node_conditions, node_ids) {
  parts = vapply(node_ids, function(id) {
    conds = node_conditions[[as.character(id)]]
    cond_sql = if (!length(conds)) "1=1" else paste(conds, collapse = " AND ")
    paste("WHEN", paste0("(", cond_sql, ")"), "THEN", id)
  }, character(1))
  paste("CASE", paste(parts, collapse = " "), "ELSE NULL END")
}

dbtree_compute_global_bins = function(
  conn,
  table_name,
  from_statement,
  xvar,
  n_bins,
  bin_method,
  bin_sample_frac,
  backend,
  where_clause,
  n_rows,
  verbose
) {
  if (is.null(bin_sample_frac)) {
    bin_sample_frac = if (n_rows > 1e6) 0.01 else 1
  }
  if (!is.numeric(bin_sample_frac) || bin_sample_frac <= 0 || bin_sample_frac > 1) {
    stop("bin_sample_frac must be NULL or a numeric value in (0,1].")
  }
  sample_n = max(10000L, ceiling(n_rows * bin_sample_frac))
  
  if (bin_sample_frac < 1) {
    if (verbose) {
      message(sprintf(
        "[dbtree] Sampling %.0f%% (%d rows) to compute bins for %s",
        bin_sample_frac * 100, sample_n, xvar
      ))
    }
    sample_sql = sql_sample(
      select_clause = xvar,
      table_name = table_name,
      where_clause = where_clause,
      n = sample_n,
      backend = backend
    )
    base_sql = glue("({sample_sql}) AS __sample")
    base_where = NULL
  } else {
    base_sql = glue("{table_name}")
    base_where = where_clause
  }
  
  if (bin_method == "quantile") {
    bin_sql = glue("
      WITH base AS (
        SELECT {xvar} AS x
        FROM {base_sql}
        {if (!is.null(base_where)) paste0('WHERE ', base_where) else ''}
      ),
      binned AS (
        SELECT x, NTILE({n_bins}) OVER (ORDER BY x) AS bin
        FROM base
      )
      SELECT bin, MIN(x) AS min_x, MAX(x) AS max_x
      FROM binned
      GROUP BY bin
      ORDER BY bin
    ")
    bins = dbGetQuery(conn, bin_sql)
    bounds = as.numeric(bins$max_x)
  } else {
    range_sql = glue("
      SELECT MIN({xvar}) AS min_x, MAX({xvar}) AS max_x
      FROM {base_sql}
      {if (!is.null(base_where)) paste0('WHERE ', base_where) else ''}
    ")
    rng = dbGetQuery(conn, range_sql)
    min_x = as.numeric(rng$min_x)
    max_x = as.numeric(rng$max_x)
    if (!is.finite(min_x) || !is.finite(max_x) || min_x == max_x) {
      bounds = max_x
    } else {
      width = (max_x - min_x) / n_bins
      bounds = min_x + width * seq_len(n_bins)
    }
  }
  
  bounds = unique(bounds)
  bounds = bounds[is.finite(bounds)]
  bounds
}

dbtree_build_bin_case = function(xvar, bounds) {
  if (length(bounds) <= 1) {
    return("1")
  }
  parts = vapply(seq_along(bounds), function(i) {
    b = format(bounds[i], digits = 15, scientific = FALSE)
    paste0("WHEN ", xvar, " <= ", b, " THEN ", i)
  }, character(1))
  paste("CASE", paste(parts, collapse = " "), "ELSE", length(bounds), "END")
}

dbtree_query_numeric_bins = function(
  conn,
  from_statement,
  node_case,
  yvar,
  xvar,
  w_expr,
  where_clause,
  bin_scope,
  n_bins,
  bin_method,
  global_bounds,
  backend
) {
  if (bin_scope == "global") {
    bin_expr = dbtree_build_bin_case("x", global_bounds)
    bin_sql = glue("
      WITH base AS (
        SELECT {node_case} AS node_id, {xvar} AS x, {yvar} AS y, {w_expr} AS w
        {from_statement}
        WHERE {where_clause}
      ),
      binned AS (
        SELECT node_id, x, y, w, {bin_expr} AS bin
        FROM base
        WHERE node_id IS NOT NULL
      )
      SELECT node_id, bin,
             {sql_count_expr(backend)} AS n,
             SUM(w) AS sum_w,
             SUM(w * y) AS sum_wy,
             SUM(w * y * y) AS sum_wy2,
             MIN(x) AS min_x,
             MAX(x) AS max_x
      FROM binned
      GROUP BY node_id, bin
      ORDER BY node_id, bin
    ")
  } else if (bin_method == "quantile") {
    bin_expr = glue("NTILE({n_bins}) OVER (PARTITION BY node_id ORDER BY x)")
    bin_sql = glue("
      WITH base AS (
        SELECT {node_case} AS node_id, {xvar} AS x, {yvar} AS y, {w_expr} AS w
        {from_statement}
        WHERE {where_clause}
      ),
      binned AS (
        SELECT node_id, x, y, w, {bin_expr} AS bin
        FROM base
        WHERE node_id IS NOT NULL
      )
      SELECT node_id, bin,
             {sql_count_expr(backend)} AS n,
             SUM(w) AS sum_w,
             SUM(w * y) AS sum_wy,
             SUM(w * y * y) AS sum_wy2,
             MIN(x) AS min_x,
             MAX(x) AS max_x
      FROM binned
      GROUP BY node_id, bin
      ORDER BY node_id, bin
    ")
  } else {
    bin_sql = glue("
      WITH base AS (
        SELECT {node_case} AS node_id, {xvar} AS x, {yvar} AS y, {w_expr} AS w
        {from_statement}
        WHERE {where_clause}
      ),
      base2 AS (
        SELECT node_id, x, y, w,
               MIN(x) OVER (PARTITION BY node_id) AS node_min,
               MAX(x) OVER (PARTITION BY node_id) AS node_max
        FROM base
        WHERE node_id IS NOT NULL
      ),
      binned AS (
        SELECT node_id, x, y, w,
               CASE
                 WHEN node_max = node_min THEN 1
                 WHEN x = node_max THEN {n_bins}
                 ELSE 1 + FLOOR((x - node_min) / NULLIF(node_max - node_min, 0) * {n_bins})
               END AS bin
        FROM base2
      )
      SELECT node_id, bin,
             {sql_count_expr(backend)} AS n,
             SUM(w) AS sum_w,
             SUM(w * y) AS sum_wy,
             SUM(w * y * y) AS sum_wy2,
             MIN(x) AS min_x,
             MAX(x) AS max_x
      FROM binned
      GROUP BY node_id, bin
      ORDER BY node_id, bin
    ")
  }
  
  stats = dbGetQuery(conn, bin_sql)
  stats
}

dbtree_query_categorical_stats = function(
  conn,
  from_statement,
  node_case,
  yvar,
  xvar,
  w_expr,
  where_clause
) {
  sql = glue("
    WITH base AS (
      SELECT {node_case} AS node_id, {xvar} AS x, {yvar} AS y, {w_expr} AS w
      {from_statement}
      WHERE {where_clause}
    )
    SELECT node_id, x,
           COUNT(*) AS n,
           SUM(w) AS sum_w,
           SUM(w * y) AS sum_wy,
           SUM(w * y * y) AS sum_wy2
    FROM base
    WHERE node_id IS NOT NULL
    GROUP BY node_id, x
    ORDER BY node_id
  ")
  dbGetQuery(conn, sql)
}

dbtree_sse = function(sum_w, sum_wy, sum_wy2) {
  sum_wy2 - (sum_wy * sum_wy) / sum_w
}

dbtree_score_numeric = function(bin_stats, total_stats, parent_sse, min_leaf, min_gain) {
  bin_stats = bin_stats[order(bin_stats$bin), , drop = FALSE]
  if (nrow(bin_stats) < 2) return(NULL)
  
  cum_n = cumsum(bin_stats$n)
  cum_sum_w = cumsum(bin_stats$sum_w)
  cum_sum_wy = cumsum(bin_stats$sum_wy)
  cum_sum_wy2 = cumsum(bin_stats$sum_wy2)
  
  idx = seq_len(nrow(bin_stats) - 1L)
  left_n = cum_n[idx]
  right_n = total_stats$n - left_n
  
  ok = left_n >= min_leaf & right_n >= min_leaf
  if (!any(ok)) return(NULL)
  
  left_sse = dbtree_sse(cum_sum_w[idx], cum_sum_wy[idx], cum_sum_wy2[idx])
  right_sum_w = total_stats$sum_w - cum_sum_w[idx]
  right_sum_wy = total_stats$sum_wy - cum_sum_wy[idx]
  right_sum_wy2 = total_stats$sum_wy2 - cum_sum_wy2[idx]
  right_sse = dbtree_sse(right_sum_w, right_sum_wy, right_sum_wy2)
  
  gain = parent_sse - (left_sse + right_sse)
  gain[!ok] = NA_real_
  best_idx = which.max(gain)
  if (!is.finite(gain[best_idx]) || gain[best_idx] < min_gain) return(NULL)
  
  split_value = as.character(bin_stats$max_x[idx][best_idx])
  left = list(
    n = left_n[best_idx],
    sum_w = cum_sum_w[idx][best_idx],
    sum_wy = cum_sum_wy[idx][best_idx],
    sum_wy2 = cum_sum_wy2[idx][best_idx]
  )
  right = list(
    n = right_n[best_idx],
    sum_w = right_sum_w[best_idx],
    sum_wy = right_sum_wy[best_idx],
    sum_wy2 = right_sum_wy2[best_idx]
  )
  list(gain = gain[best_idx], split_value = split_value, left = left, right = right)
}

dbtree_score_categorical = function(cat_stats, total_stats, parent_sse, min_leaf, min_gain) {
  if (nrow(cat_stats) < 2) return(NULL)
  
  left_n = cat_stats$n
  right_n = total_stats$n - left_n
  ok = left_n >= min_leaf & right_n >= min_leaf
  if (!any(ok)) return(NULL)
  
  left_sse = dbtree_sse(cat_stats$sum_w, cat_stats$sum_wy, cat_stats$sum_wy2)
  right_sum_w = total_stats$sum_w - cat_stats$sum_w
  right_sum_wy = total_stats$sum_wy - cat_stats$sum_wy
  right_sum_wy2 = total_stats$sum_wy2 - cat_stats$sum_wy2
  right_sse = dbtree_sse(right_sum_w, right_sum_wy, right_sum_wy2)
  
  gain = parent_sse - (left_sse + right_sse)
  gain[!ok] = NA_real_
  best_idx = which.max(gain)
  if (!is.finite(gain[best_idx]) || gain[best_idx] < min_gain) return(NULL)
  
  split_value = as.character(cat_stats$x[best_idx])
  left = list(
    n = left_n[best_idx],
    sum_w = cat_stats$sum_w[best_idx],
    sum_wy = cat_stats$sum_wy[best_idx],
    sum_wy2 = cat_stats$sum_wy2[best_idx]
  )
  right = list(
    n = right_n[best_idx],
    sum_w = right_sum_w[best_idx],
    sum_wy = right_sum_wy[best_idx],
    sum_wy2 = right_sum_wy2[best_idx]
  )
  list(gain = gain[best_idx], split_value = split_value, left = left, right = right)
}
