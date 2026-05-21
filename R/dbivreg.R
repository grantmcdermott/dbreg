#' Instrumental-variables regression on a database backend
#'
#' @md
#' @description
#' Estimates a linear IV model via exact sufficient statistics on a database
#' backend. The current implementation supports two exact strategies:
#'
#' - `strategy = "moments"` for models without absorbed fixed effects.
#' - `strategy = "demean"` for one or more absorbed fixed effects.
#'
#' Multi-way fixed effects use exact alternating projections when there are
#' more than two fixed effects, or when two-way fixed effects are unbalanced or
#' weights are supplied.
#'
#' @param fml A fixest-style IV formula. Use `y ~ x1 + x2 | d ~ z` for models
#'   without absorbed fixed effects, and `y ~ x1 + x2 | fe1 + fe2 + fe3 | d ~ z`
#'   when fixed effects are present. The IV block must always be the final
#'   pipe-separated block.
#' @param conn Database connection, or `NULL` to create an ephemeral DuckDB
#'   connection.
#' @param table,data,path Mutually exclusive data source arguments with
#'   precedence `table > data > path`.
#' @param weights Optional character string naming an observation-weight column.
#' @param vcov Character string or one-sided clustering formula. Character
#'   options are `"iid"` and `"hc1"`.
#' @param strategy Character string selecting the IV backend. Supported values
#'   are `"auto"` (default), `"moments"`, `"demean"` (alias `"within"`),
#'   `"compress"`, and `"mundlak"`. The latter two currently error because they
#'   are not yet implemented for IV.
#' @param cluster Optional clustering variable, supplied as a one-sided formula
#'   or character string.
#' @param sql_only Logical indicating whether to return only the underlying SQL
#'   query.
#' @param data_only Logical indicating whether to return only the aggregated
#'   sufficient statistics.
#' @param drop_missings Logical indicating whether incomplete cases should be
#'   dropped.
#' @param verbose Logical indicating whether progress messages are printed.
#' @param ... Additional unused arguments.
#'
#' @return A list of class `"dbivreg"` and `"dbreg"`.
#' @export
dbivreg = function(
  fml,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  weights = NULL,
  vcov = c("iid", "hc1"),
  strategy = c("auto", "moments", "demean", "within", "compress", "mundlak"),
  cluster = NULL,
  sql_only = FALSE,
  data_only = FALSE,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  verbose = isTRUE(verbose)
  strategy = match.arg(strategy)
  if (strategy == "within") {
    strategy = "demean"
  }
  dots = list(...)
  if (length(dots)) {
    dot_names = names(dots)
    if (is.null(dot_names)) {
      dot_names = rep("", length(dots))
    }
    legacy_args = intersect(dot_names, c("endog", "instruments"))
    if (length(legacy_args) > 0) {
      stop(
        "`endog` and `instruments` are no longer supported. ",
        "Use fixest-style IV formulas like `y ~ x | d ~ z` or `y ~ x | fe1 + fe2 | d ~ z`."
      )
    }
    stop(
      "Unused arguments: ",
      paste(ifelse(nzchar(dot_names), paste0("`", dot_names, "`"), "<unnamed>"), collapse = ", ")
    )
  }

  vcov_parsed = parse_vcov_args(vcov, cluster, valid_types = c("iid", "hc1"))
  inputs = process_dbivreg_inputs(
    fml = fml,
    conn = conn,
    table = table,
    data = data,
    path = path,
    weights = weights,
    vcov = vcov_parsed[["vcov_type"]],
    cluster = vcov_parsed[["cluster_var"]],
    strategy = strategy,
    sql_only = sql_only,
    data_only = data_only,
    drop_missings = drop_missings,
    verbose = verbose
  )

  cleanup_conn = function() {
    if (isTRUE(inputs[["own_conn"]]) && dbIsValid(inputs[["conn"]])) {
      dbDisconnect(inputs[["conn"]], shutdown = TRUE)
    }
  }
  on.exit(cleanup_conn(), add = TRUE)

  cleanup_registered = function() {
    if (!is.null(inputs[["registered_table"]]) && dbIsValid(inputs[["conn"]])) {
      try(duckdb_unregister(inputs[["conn"]], inputs[["registered_table"]]), silent = TRUE)
    }
  }
  on.exit(cleanup_registered(), add = TRUE)

  chosen_strategy = choose_dbivreg_strategy(inputs)

  result = switch(
    chosen_strategy,
    "moments" = execute_iv_moments_strategy(inputs),
    "demean" = execute_iv_demean_strategy(inputs),
    stop("Unknown IV strategy: ", chosen_strategy)
  )

  if (inputs[["sql_only"]]) {
    cat(result)
    return(invisible(result))
  }
  if (inputs[["data_only"]]) {
    return(result)
  }

  result[["strategy"]] = chosen_strategy
  class(result) = c("dbivreg", "dbreg")
  result
}

#' Process and validate dbivreg inputs
#'
#' Returns an environment so downstream strategy helpers follow the post-#65
#' dbreg convention of reading shared state with `inputs[["..."]]`.
#'
#' @keywords internal
process_dbivreg_inputs = function(
  fml,
  conn,
  table,
  data,
  path,
  weights,
  vcov,
  cluster,
  strategy,
  sql_only,
  data_only,
  drop_missings,
  verbose
) {
  vcov_type_req = vcov
  cluster_var = cluster

  parsed_fml = dbivreg_parse_formula(fml)
  yvar = parsed_fml[["yvar"]]
  fe = parsed_fml[["fe"]]

  db_setup = setup_db_connection(conn, table, data, path, caller = "dbivreg")
  conn = db_setup[["conn"]]
  own_conn = db_setup[["own_conn"]]
  from_statement = db_setup[["from_statement"]]
  registered_table = db_setup[["registered_table"]]

  if (!is.null(weights)) {
    if (!is.character(weights) || length(weights) != 1) {
      stop("`weights` must be a single character string (column name) or NULL.")
    }
    if (!is.null(data)) {
      if (!weights %in% names(data)) {
        stop("Weight column '", weights, "' not found in data.")
      }
      wv = data[[weights]]
      if (any(!is.finite(wv) & !is.na(wv))) {
        stop("Weights must be finite.")
      }
      if (any(wv < 0, na.rm = TRUE)) {
        stop("Weights must be non-negative.")
      }
    } else {
      neg_sql = glue(
        "SELECT 1 FROM (SELECT * {from_statement}) t WHERE {weights} < 0 LIMIT 1"
      )
      has_neg = tryCatch(nrow(dbGetQuery(conn, neg_sql)) > 0, error = function(e) FALSE)
      if (isTRUE(has_neg)) {
        stop("Weights must be non-negative.")
      }
    }
  }

  if (isTRUE(drop_missings) || !is.null(weights)) {
    if (grepl("WHERE|LIMIT|ORDER\\s+BY|GROUP\\s+BY|HAVING", from_statement, ignore.case = TRUE)) {
      from_statement = glue("FROM (SELECT * {from_statement}) AS subq")
    }
    where_clauses = character(0)
    if (isTRUE(drop_missings)) {
      miss_vars = unique(c(
        yvar,
        parsed_fml[["exog_vars"]],
        parsed_fml[["endog_vars"]],
        parsed_fml[["instr_vars"]],
        fe,
        cluster_var
      ))
      miss_vars = miss_vars[!is.na(miss_vars) & nzchar(miss_vars)]
      where_clauses = c(where_clauses, paste0(miss_vars, " IS NOT NULL"))
    }
    if (!is.null(weights)) {
      where_clauses = c(where_clauses, paste0(weights, " > 0"))
    }
    if (length(where_clauses) > 0) {
      from_statement = glue("
      {from_statement}
      WHERE {paste(where_clauses, collapse = ' AND ')}
      ")
    }
  }

  design_source = sub("^FROM\\s+", "", from_statement, ignore.case = TRUE)
  exog_design = dbivreg_expand_block(parsed_fml[["exog_rhs"]], conn, design_source, fe)
  endog_design = dbivreg_expand_block(parsed_fml[["endog_rhs"]], conn, design_source, fe)
  instr_design = dbivreg_expand_block(parsed_fml[["instr_rhs"]], conn, design_source, fe)

  if (!length(endog_design[["names"]])) {
    stop("At least one endogenous regressor is required in the IV block.")
  }
  if (!length(instr_design[["names"]])) {
    stop("At least one excluded instrument is required in the IV block.")
  }

  overlap_x_endog = intersect(exog_design[["names"]], endog_design[["names"]])
  if (length(overlap_x_endog) > 0) {
    stop(
      "Endogenous regressors cannot also appear as included exogenous regressors. ",
      "Overlapping term(s): ",
      paste(standardize_coef_names(overlap_x_endog), collapse = ", ")
    )
  }
  overlap_endog_iv = intersect(endog_design[["names"]], instr_design[["names"]])
  if (length(overlap_endog_iv) > 0) {
    stop(
      "Excluded instruments cannot also appear as endogenous regressors. ",
      "Overlapping term(s): ",
      paste(standardize_coef_names(overlap_endog_iv), collapse = ", ")
    )
  }
  overlap_exog_iv = intersect(exog_design[["names"]], instr_design[["names"]])
  if (length(overlap_exog_iv) > 0) {
    warning(
      "Included exogenous regressors are already valid instruments. ",
      "Dropping duplicate entries from the excluded-instrument block: ",
      paste(standardize_coef_names(overlap_exog_iv), collapse = ", ")
    )
    keep_iv = !instr_design[["names"]] %in% overlap_exog_iv
    instr_design[["sql"]] = instr_design[["sql"]][keep_iv]
    instr_design[["names"]] = instr_design[["names"]][keep_iv]
  }
  if (length(instr_design[["names"]]) < length(endog_design[["names"]])) {
    stop("Need at least as many excluded instrument columns as endogenous regressor columns.")
  }

  x_all_sql = c(exog_design[["sql"]], endog_design[["sql"]])
  x_all_names = c(exog_design[["names"]], endog_design[["names"]])
  z_all_names = c(exog_design[["names"]], instr_design[["names"]])

  union_map = c(
    stats::setNames(x_all_sql, x_all_names),
    stats::setNames(instr_design[["sql"]], instr_design[["names"]])
  )
  union_map = union_map[!duplicated(names(union_map))]
  union_names = names(union_map)
  union_sql = unname(union_map)

  list2env(list(
    fml = fml,
    structural_fml = dbivreg_structural_formula(
      yvar = yvar,
      exog_rhs = parsed_fml[["exog_rhs_text"]],
      endog_rhs = parsed_fml[["endog_rhs_text"]],
      fe_rhs = parsed_fml[["fe_rhs_text"]],
      env = parsed_fml[["env"]]
    ),
    yvar = yvar,
    fe = fe,
    exog_names = exog_design[["names"]],
    exog_sql = exog_design[["sql"]],
    endog_names = endog_design[["names"]],
    endog_sql = endog_design[["sql"]],
    instr_names = instr_design[["names"]],
    instr_sql = instr_design[["sql"]],
    endog_vars = standardize_coef_names(endog_design[["names"]]),
    instr_vars = standardize_coef_names(instr_design[["names"]]),
    x_all_names = x_all_names,
    z_all_names = z_all_names,
    union_names = union_names,
    union_sql = union_sql,
    conn = conn,
    from_statement = from_statement,
    weights = weights,
    vcov_type_req = vcov_type_req,
    cluster_var = cluster_var,
    strategy = strategy,
    sql_only = sql_only,
    data_only = data_only,
    verbose = verbose,
    own_conn = own_conn,
    registered_table = registered_table
  ), parent = emptyenv())
}

#' @keywords internal
choose_dbivreg_strategy = function(inputs) {
  strategy = inputs[["strategy"]]
  fe = inputs[["fe"]]
  verbose = inputs[["verbose"]]

  if (strategy == "auto") {
    chosen_strategy = if (length(fe) == 0) "moments" else "demean"
    if (verbose) {
      message("[dbivreg] Auto strategy: ", chosen_strategy)
    }
  } else {
    chosen_strategy = strategy
    if (verbose) {
      message("[dbivreg] Using strategy: ", chosen_strategy)
    }
  }

  if (chosen_strategy %in% c("compress", "mundlak")) {
    stop(
      "dbivreg() does not yet implement strategy = '",
      chosen_strategy,
      "'. Supported IV strategies are 'moments' and 'demean'."
    )
  }
  if (chosen_strategy == "moments" && length(fe) > 0) {
    stop(
      "strategy = 'moments' is only available without fixed effects. ",
      "Use strategy = 'demean' or strategy = 'auto'."
    )
  }
  if (chosen_strategy == "demean" && length(fe) < 1) {
    stop(
      "strategy = 'demean' requires at least one fixed effect. ",
      "Use strategy = 'moments' or strategy = 'auto' for models without fixed effects."
    )
  }

  chosen_strategy
}

#' @keywords internal
execute_iv_moments_strategy = function(inputs) {
  weight_alias = "__dbivreg_weight"
  base_select = c(sprintf("%s AS %s", inputs[["yvar"]], inputs[["yvar"]]))
  for (i in seq_along(inputs[["union_names"]])) {
    if (identical(inputs[["union_sql"]][i], inputs[["union_names"]][i])) {
      base_select = c(base_select, inputs[["union_names"]][i])
    } else {
      base_select = c(base_select, sprintf("%s AS %s", inputs[["union_sql"]][i], inputs[["union_names"]][i]))
    }
  }
  if (!is.null(inputs[["weights"]])) {
    base_select = c(base_select, sprintf("%s AS %s", inputs[["weights"]], weight_alias))
  }
  if (!is.null(inputs[["cluster_var"]]) && !inputs[["cluster_var"]] %in% c(inputs[["yvar"]], inputs[["union_names"]], weight_alias)) {
    base_select = c(base_select, inputs[["cluster_var"]])
  }

  weights_expr = if (is.null(inputs[["weights"]])) NULL else sql_weight_expr(weight_alias)
  moment_terms = dbivreg_union_moment_terms(
    conn = inputs[["conn"]],
    union_names = inputs[["union_names"]],
    y_expr = inputs[["yvar"]],
    weights_expr = weights_expr,
    has_intercept = TRUE
  )

  cte_sql = paste0(
    "WITH base AS (\n  SELECT ",
    paste(base_select, collapse = ", "),
    " ",
    inputs[["from_statement"]],
    "\n)"
  )
  moments_sql = paste0(
    cte_sql,
    "\nSELECT\n  ",
    paste(moment_terms, collapse = ",\n  "),
    "\nFROM base"
  )

  if (inputs[["sql_only"]]) {
    return(moments_sql)
  }
  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbivreg] Executing weighted IV moments SQL\n" else "[dbivreg] Executing IV moments SQL\n")
  }

  moments_df = dbGetQuery(inputs[["conn"]], moments_sql)
  if (inputs[["data_only"]]) {
    return(moments_df)
  }

  dbivreg_finalize_fit(
    inputs = inputs,
    moment_df = moments_df,
    query_string = moments_sql,
    cte_sql = cte_sql,
    cte_name = "base",
    x_names_full = c("(Intercept)", inputs[["x_all_names"]]),
    z_names_full = c("(Intercept)", inputs[["z_all_names"]]),
    has_intercept = TRUE,
    weights_expr = weights_expr,
    x_vars_sql = inputs[["x_all_names"]],
    z_vars_sql = inputs[["z_all_names"]],
    yvar_sql = inputs[["yvar"]]
  )
}

#' @keywords internal
execute_iv_demean_strategy = function(inputs) {
  yvar = inputs[["yvar"]]
  fe = inputs[["fe"]]
  union_names = inputs[["union_names"]]
  weights_expr_base = sql_weight_expr(inputs[["weights"]])
  weights_expr_demeaned = if (is.null(inputs[["weights"]])) NULL else sql_weight_expr("weights")

  all_var_names = c(yvar, union_names)
  cluster_var = inputs[["cluster_var"]]
  ap_tables = NULL

  use_ap = FALSE
  if (length(fe) >= 2) {
    if (length(fe) > 2) {
      use_ap = TRUE
    } else {
      is_balanced = dbreg_is_balanced_panel(inputs[["conn"]], inputs[["from_statement"]], fe)
      use_ap = !is.null(inputs[["weights"]]) || !isTRUE(is_balanced)
    }
    if (isTRUE(use_ap) && inputs[["verbose"]]) {
      message("[dbivreg] Using alternating projections for FE demeaning")
    }
    if (isTRUE(use_ap) && inputs[["sql_only"]]) {
      stop("[dbivreg] sql_only is not supported for alternating projections.", call. = FALSE)
    }
  }

  if (length(fe) == 1) {
    fe1 = fe[1]
    base_select = c(fe1, yvar)
    for (i in seq_along(union_names)) {
      base_select = c(base_select, sprintf("%s AS %s", inputs[["union_sql"]][i], union_names[i]))
    }
    if (!is.null(inputs[["weights"]]) && !inputs[["weights"]] %in% c(fe1, yvar, union_names)) {
      base_select = c(base_select, inputs[["weights"]])
    }
    if (!is.null(cluster_var) && !cluster_var %in% c(fe1, yvar, union_names, inputs[["weights"]])) {
      base_select = c(base_select, cluster_var)
    }

    means_cols = paste(
      vapply(
        all_var_names,
        function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_mean")),
        character(1)
      ),
      collapse = ", "
    )
    tilde_exprs = paste(
      sprintf("(b.%s - gm.%s_mean) AS %s_tilde", all_var_names, all_var_names, all_var_names),
      collapse = ",\n       "
    )
    if (!is.null(inputs[["weights"]])) {
      tilde_exprs = paste(
        tilde_exprs,
        sprintf("b.%s AS weights", inputs[["weights"]]),
        sep = ",\n       "
      )
    }
    if (!is.null(cluster_var) && !cluster_var %in% fe1) {
      tilde_exprs = paste(
        tilde_exprs,
        sprintf("b.%s AS %s", cluster_var, cluster_var),
        sep = ",\n       "
      )
    }

    cte_sql = paste0(
      "WITH base AS (
      SELECT ", paste(base_select, collapse = ", "), " ",
      inputs[["from_statement"]],
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
  } else {
    fe1 = fe[1]
    fe2 = fe[2]

    if (isTRUE(use_ap)) {
      ap_res = dbreg_alternating_projections(
        conn = inputs[["conn"]],
        from_statement = inputs[["from_statement"]],
        fe = fe,
        yvar = yvar,
        xvars_sql = inputs[["union_sql"]],
        xvar_names = union_names,
        weights = inputs[["weights"]],
        cluster_var = cluster_var,
        verbose = inputs[["verbose"]]
      )
      ap_tables = c(ap_res[["table"]], ap_res[["base_table"]])
      weights_expr_demeaned = "__w"
      cte_sql = paste0("WITH demeaned AS (SELECT * FROM ", ap_res[["table"]], ")")
    } else {
      base_select = c(fe1, fe2, yvar)
      for (i in seq_along(union_names)) {
        base_select = c(base_select, sprintf("%s AS %s", inputs[["union_sql"]][i], union_names[i]))
      }
      if (!is.null(inputs[["weights"]]) && !inputs[["weights"]] %in% c(fe1, fe2, yvar, union_names)) {
        base_select = c(base_select, inputs[["weights"]])
      }
      if (!is.null(cluster_var) && !cluster_var %in% c(fe1, fe2, yvar, union_names, inputs[["weights"]])) {
        base_select = c(base_select, cluster_var)
      }

      unit_means_cols = paste(
        vapply(
          all_var_names,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_u")),
          character(1)
        ),
        collapse = ", "
      )
      time_means_cols = paste(
        vapply(
          all_var_names,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_t")),
          character(1)
        ),
        collapse = ", "
      )
      overall_cols = paste(
        vapply(
          all_var_names,
          function(v) sql_weighted_mean(v, weights_expr_base, paste0(v, "_o")),
          character(1)
        ),
        collapse = ", "
      )
      tilde_exprs = paste(
        sprintf(
          "(b.%s - um.%s_u - tm.%s_t + o.%s_o) AS %s_tilde",
          all_var_names,
          all_var_names,
          all_var_names,
          all_var_names,
          all_var_names
        ),
        collapse = ",\n       "
      )
      if (!is.null(cluster_var) && !cluster_var %in% c(fe1, fe2)) {
        tilde_exprs = paste(
          tilde_exprs,
          sprintf("b.%s AS %s", cluster_var, cluster_var),
          sep = ",\n       "
        )
      }
      if (!is.null(inputs[["weights"]])) {
        tilde_exprs = paste(
          tilde_exprs,
          sprintf("b.%s AS weights", inputs[["weights"]]),
          sep = ",\n       "
        )
      }

      cte_sql = paste0(
        "WITH base AS (
        SELECT ", paste(base_select, collapse = ", "), " ",
        inputs[["from_statement"]],
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
    }
  }

  fe_count_terms = vapply(seq_along(fe), function(k) {
    sql_count(inputs[["conn"]], sprintf("n_fe%d", k), fe[k], distinct = TRUE)
  }, character(1))
  if (length(fe) == 1) {
    fe_count_terms = c(fe_count_terms, "1 AS n_fe2")
  }
  moment_terms = c(
    sql_count(inputs[["conn"]], "n_total"),
    fe_count_terms,
    sql_weighted_sum(
      glue("CAST({yvar}_tilde AS FLOAT) * CAST({yvar}_tilde AS FLOAT)"),
      weights_expr_demeaned,
      "sum_y_sq"
    )
  )
  for (i in seq_along(union_names)) {
    moment_terms = c(
      moment_terms,
      sql_weighted_sum(
        glue("CAST({union_names[i]}_tilde AS FLOAT) * CAST({yvar}_tilde AS FLOAT)"),
        weights_expr_demeaned,
        sprintf("sum_u%d_y", i)
      ),
      sql_weighted_sum(
        glue("CAST({union_names[i]}_tilde AS FLOAT) * CAST({union_names[i]}_tilde AS FLOAT)"),
        weights_expr_demeaned,
        sprintf("sum_u%d_%d", i, i)
      )
    )
  }
  if (length(union_names) > 1) {
    for (i in seq_along(union_names)) {
      if (i == 1) {
        next
      }
      for (j in seq_len(i - 1)) {
        moment_terms = c(
          moment_terms,
          sql_weighted_sum(
            glue("CAST({union_names[j]}_tilde AS FLOAT) * CAST({union_names[i]}_tilde AS FLOAT)"),
            weights_expr_demeaned,
            sprintf("sum_u%d_%d", j, i)
          )
        )
      }
    }
  }

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

  if (inherits(inputs[["conn"]], "AthenaConnection")) {
    demean_sql = gsub("FLOAT", "REAL", demean_sql, fixed = TRUE)
  }
  if (inputs[["sql_only"]]) {
    return(demean_sql)
  }
  if (inputs[["verbose"]]) {
    message(if (!is.null(inputs[["weights"]])) "[dbivreg] Executing weighted demeaned IV SQL\n" else "[dbivreg] Executing demeaned IV SQL\n")
  }

  ap_cleanup = function() {
    if (!is.null(ap_tables)) {
      backend = detect_backend(inputs[["conn"]])[["name"]]
      for (tbl in ap_tables) {
        drop_table_if_exists(inputs[["conn"]], tbl, backend)
      }
    }
  }
  on.exit(ap_cleanup(), add = TRUE)

  demean_df = dbGetQuery(inputs[["conn"]], demean_sql)
  if (inputs[["data_only"]]) {
    return(demean_df)
  }

  dbivreg_finalize_fit(
    inputs = inputs,
    moment_df = demean_df,
    query_string = demean_sql,
    cte_sql = cte_sql,
    cte_name = "demeaned",
    x_names_full = inputs[["x_all_names"]],
    z_names_full = inputs[["z_all_names"]],
    has_intercept = FALSE,
    weights_expr = weights_expr_demeaned,
    x_vars_sql = paste0(inputs[["x_all_names"]], "_tilde"),
    z_vars_sql = paste0(inputs[["z_all_names"]], "_tilde"),
    yvar_sql = paste0(inputs[["yvar"]], "_tilde")
  )
}

#' @keywords internal
dbivreg_finalize_fit = function(
  inputs,
  moment_df,
  query_string,
  cte_sql,
  cte_name,
  x_names_full,
  z_names_full,
  has_intercept,
  weights_expr,
  x_vars_sql,
  z_vars_sql,
  yvar_sql
) {
  moms = dbivreg_reconstruct_moments(moment_df, inputs[["union_names"]], has_intercept)

  S_xx = moms$S_uu[x_names_full, x_names_full, drop = FALSE]
  S_xy = moms$S_uy[x_names_full, , drop = FALSE]
  S_zz = moms$S_uu[z_names_full, z_names_full, drop = FALSE]
  S_zx = moms$S_uu[z_names_full, x_names_full, drop = FALSE]
  S_zy = moms$S_uy[z_names_full, , drop = FALSE]

  if (qr(S_zz, tol = 1e-10)$rank < ncol(S_zz)) {
    stop("Instrument matrix is rank deficient.")
  }
  z_inv = solve_with_fallback(S_zz, diag(ncol(S_zz)))$XtX_inv
  A = Matrix::t(S_zx) %*% z_inv %*% S_zx
  if (qr(A, tol = 1e-10)$rank < ncol(A)) {
    stop("IV system is not identified or regressors are collinear after instrumentation.")
  }
  rhs = Matrix::t(S_zx) %*% z_inv %*% S_zy
  solve_result = solve_with_fallback(A, rhs)
  betahat = solve_result$betahat
  A_inv = solve_result$XtX_inv
  rownames(betahat) = x_names_full

  rss = as.numeric(
    moms$sum_y_sq -
      2 * Matrix::t(betahat) %*% S_xy +
      Matrix::t(betahat) %*% S_xx %*% betahat
  )

  n_fe = length(inputs[["fe"]])
  n_fe_levels = if (n_fe == 0) {
    numeric(0)
  } else {
    stats::setNames(
      vapply(seq_len(n_fe), function(k) {
        val = moment_df[[sprintf("n_fe%d", k)]]
        if (is.null(val)) 1 else as.numeric(val)
      }, numeric(1)),
      inputs[["fe"]]
    )
  }
  df_fe = if (n_fe == 0) 0 else sum(n_fe_levels) - (n_fe - 1)
  df_res = max(moment_df$n_total - length(x_names_full) - df_fe, 1)
  n_params = length(x_names_full) + df_fe

  tss = if (has_intercept) {
    moms$sum_y_sq - (moms$sum_wy^2 / moms$sum_w)
  } else {
    moms$sum_y_sq
  }

  meat = NULL
  is_athena = inherits(inputs[["conn"]], "AthenaConnection")
  if (inputs[["vcov_type_req"]] == "hc1") {
    meat = compute_iv_meat_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      x_vars = x_names_full[!x_names_full %in% "(Intercept)"],
      z_vars = z_names_full[!z_names_full %in% "(Intercept)"],
      yvar = inputs[["yvar"]],
      betahat = betahat,
      weights_expr = weights_expr,
      cte_name = cte_name,
      has_intercept = has_intercept,
      is_athena = is_athena,
      x_vars_sql = x_vars_sql,
      z_vars_sql = z_vars_sql,
      yvar_sql = yvar_sql
    )
  } else if (inputs[["vcov_type_req"]] == "cluster") {
    meat = compute_iv_meat_cluster_sql(
      conn = inputs[["conn"]],
      cte_sql = cte_sql,
      x_vars = x_names_full[!x_names_full %in% "(Intercept)"],
      z_vars = z_names_full[!z_names_full %in% "(Intercept)"],
      yvar = inputs[["yvar"]],
      betahat = betahat,
      cluster_var = inputs[["cluster_var"]],
      weights_expr = weights_expr,
      cte_name = cte_name,
      has_intercept = has_intercept,
      is_athena = is_athena,
      x_vars_sql = x_vars_sql,
      z_vars_sql = z_vars_sql,
      yvar_sql = yvar_sql
    )
  }

  vcov_mat = compute_iv_vcov(
    vcov_type = inputs[["vcov_type_req"]],
    A_inv = A_inv,
    ZtZ_inv = z_inv,
    ZtX = S_zx,
    rss = rss,
    df_res = df_res,
    nobs_orig = moment_df$n_total,
    n_params = n_params,
    meat = meat
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)
  iv_diagnostics = compute_dbivreg_diagnostics(
    inputs = inputs,
    moms = moms,
    cte_sql = cte_sql,
    cte_name = cte_name,
    has_intercept = has_intercept,
    weights_expr = weights_expr,
    n_total = moment_df$n_total,
    df_fe = df_fe,
    rss = rss,
    betahat = betahat,
    S_zz = S_zz,
    S_zx = S_zx,
    S_zy = S_zy,
    meat = meat
  )

  out = list(
    coeftable = coeftable,
    vcov = vcov_mat,
    fml = inputs[["structural_fml"]],
    yvar = inputs[["yvar"]],
    xvars = standardize_coef_names(x_names_full[x_names_full != "(Intercept)"]),
    fe = inputs[["fe"]],
    weights = inputs[["weights"]],
    query_string = query_string,
    nobs = 1L,
    nobs_orig = moment_df$n_total,
    estimator = "2SLS",
    endogenous = inputs[["endog_vars"]],
    instruments = inputs[["instr_vars"]],
    diagnostics = iv_diagnostics,
    df_residual = df_res,
    n_fe_levels = if (n_fe > 0) n_fe_levels else NULL,
    n_fe1 = if ("n_fe1" %in% names(moment_df)) moment_df$n_fe1 else NULL,
    n_fe2 = if ("n_fe2" %in% names(moment_df)) moment_df$n_fe2 else NULL
  )
  if (n_fe > 2) {
    for (k in seq.int(3L, n_fe)) {
      out[[sprintf("n_fe%d", k)]] = unname(n_fe_levels[k])
    }
  }
  out
}

#' @keywords internal
dbivreg_reconstruct_moments = function(moment_df, union_names, has_intercept) {
  if (has_intercept) {
    union_all = c("(Intercept)", union_names)
    S_uu = matrix(0, length(union_all), length(union_all), dimnames = list(union_all, union_all))
    S_uy = matrix(0, length(union_all), 1, dimnames = list(union_all, ""))

    S_uu["(Intercept)", "(Intercept)"] = moment_df$sum_w
    S_uy["(Intercept)", ] = moment_df$sum_wy
    for (i in seq_along(union_names)) {
      S_uu["(Intercept)", union_names[i]] = S_uu[union_names[i], "(Intercept)"] = moment_df[[sprintf("sum_u%d", i)]]
      S_uu[union_names[i], union_names[i]] = moment_df[[sprintf("sum_u%d_%d", i, i)]]
      S_uy[union_names[i], ] = moment_df[[sprintf("sum_u%d_y", i)]]
    }
    if (length(union_names) > 1) {
      for (i in seq_along(union_names)) {
        if (i == 1) {
          next
        }
        for (j in seq_len(i - 1)) {
          S_uu[union_names[i], union_names[j]] = S_uu[union_names[j], union_names[i]] = moment_df[[sprintf("sum_u%d_%d", j, i)]]
        }
      }
    }
    list(
      S_uu = S_uu,
      S_uy = S_uy,
      sum_w = moment_df$sum_w,
      sum_wy = moment_df$sum_wy,
      sum_y_sq = moment_df$sum_wy_sq
    )
  } else {
    S_uu = matrix(0, length(union_names), length(union_names), dimnames = list(union_names, union_names))
    S_uy = matrix(0, length(union_names), 1, dimnames = list(union_names, ""))

    for (i in seq_along(union_names)) {
      S_uu[i, i] = moment_df[[sprintf("sum_u%d_%d", i, i)]]
      S_uy[i, ] = moment_df[[sprintf("sum_u%d_y", i)]]
    }
    if (length(union_names) > 1) {
      for (i in seq_along(union_names)) {
        if (i == 1) {
          next
        }
        for (j in seq_len(i - 1)) {
          S_uu[i, j] = S_uu[j, i] = moment_df[[sprintf("sum_u%d_%d", j, i)]]
        }
      }
    }
    list(
      S_uu = S_uu,
      S_uy = S_uy,
      sum_w = NA_real_,
      sum_wy = NA_real_,
      sum_y_sq = moment_df$sum_y_sq
    )
  }
}

#' @keywords internal
dbivreg_union_moment_terms = function(conn, union_names, y_expr, weights_expr, has_intercept) {
  terms = c(
    sql_count(conn, "n_total"),
    if (is.null(weights_expr)) sql_count(conn, "sum_w") else glue("SUM({weights_expr}) AS sum_w"),
    sql_weighted_sum(y_expr, weights_expr, "sum_wy"),
    sql_weighted_sum(glue("({y_expr}) * ({y_expr})"), weights_expr, "sum_wy_sq")
  )
  for (i in seq_along(union_names)) {
    if (has_intercept) {
      terms = c(terms, sql_weighted_sum(union_names[i], weights_expr, sprintf("sum_u%d", i)))
    }
    terms = c(
      terms,
      sql_weighted_sum(glue("({union_names[i]}) * ({y_expr})"), weights_expr, sprintf("sum_u%d_y", i)),
      sql_weighted_sum(glue("({union_names[i]}) * ({union_names[i]})"), weights_expr, sprintf("sum_u%d_%d", i, i))
    )
  }
  if (length(union_names) > 1) {
    for (i in seq_along(union_names)) {
      if (i == 1) {
        next
      }
      for (j in seq_len(i - 1)) {
        terms = c(
          terms,
          sql_weighted_sum(glue("({union_names[j]}) * ({union_names[i]})"), weights_expr, sprintf("sum_u%d_%d", j, i))
        )
      }
    }
  }
  terms
}

#' @keywords internal
dbivreg_structural_formula = function(yvar, exog_rhs, endog_rhs, fe_rhs = NULL, env = parent.frame()) {
  rhs_parts = c(dbivreg_trim_ws(exog_rhs), dbivreg_trim_ws(endog_rhs))
  rhs_parts = rhs_parts[nzchar(rhs_parts)]
  rhs = paste(rhs_parts, collapse = " + ")
  if (is.null(fe_rhs) || !nzchar(dbivreg_trim_ws(fe_rhs))) {
    return(Formula(stats::as.formula(paste(yvar, "~", rhs), env = env)))
  }
  Formula(stats::as.formula(
    paste(yvar, "~", rhs, "|", dbivreg_trim_ws(fe_rhs)),
    env = env
  ))
}

#' @keywords internal
dbivreg_parse_formula = function(fml) {
  if (inherits(fml, "Formula")) {
    fml = formula(fml)
  }
  if (!inherits(fml, "formula")) {
    stop("`fml` must be a formula.")
  }

  env = environment(fml)
  fml_text = paste(deparse(fml, width.cutoff = 500L), collapse = " ")
  main_tilde = dbivreg_find_top_level(fml_text, "~")
  if (is.na(main_tilde)) {
    stop(
      "dbivreg() requires a fixest-style IV formula: ",
      "`y ~ x | d ~ z` or `y ~ x | fe1 + fe2 | d ~ z`."
    )
  }

  lhs_text = dbivreg_trim_ws(substr(fml_text, 1L, main_tilde - 1L))
  rhs_text = dbivreg_trim_ws(substr(fml_text, main_tilde + 1L, nchar(fml_text)))
  pipe_parts = dbivreg_split_top_level(rhs_text, "|")
  if (!(length(pipe_parts) %in% c(2L, 3L))) {
    stop(
      "dbivreg() requires a fixest-style IV formula with the IV block last: ",
      "`y ~ x | d ~ z` or `y ~ x | fe1 + fe2 | d ~ z`."
    )
  }

  exog_rhs_text = pipe_parts[1]
  fe_rhs_text = if (length(pipe_parts) == 3L) pipe_parts[2] else NULL
  iv_rhs_text = pipe_parts[length(pipe_parts)]

  iv_tilde = dbivreg_find_top_level(iv_rhs_text, "~")
  if (is.na(iv_tilde)) {
    stop(
      "The final pipe-separated block must be the IV specification, e.g. `d ~ z`."
    )
  }

  endog_rhs_text = dbivreg_trim_ws(substr(iv_rhs_text, 1L, iv_tilde - 1L))
  instr_rhs_text = dbivreg_trim_ws(substr(iv_rhs_text, iv_tilde + 1L, nchar(iv_rhs_text)))
  if (!nzchar(endog_rhs_text) || !nzchar(instr_rhs_text)) {
    stop("The IV block must contain both endogenous regressors and excluded instruments.")
  }

  y_formula = stats::as.formula(paste(lhs_text, "~ 1"), env = env)
  yvar = all.vars(y_formula)
  if (length(yvar) != 1L) {
    stop("Exactly one outcome variable required.")
  }

  exog_rhs = stats::as.formula(paste("~", exog_rhs_text), env = env)
  endog_rhs = stats::as.formula(paste("~", endog_rhs_text), env = env)
  instr_rhs = stats::as.formula(paste("~", instr_rhs_text), env = env)

  fe = NULL
  if (!is.null(fe_rhs_text)) {
    fe_rhs = stats::as.formula(paste("~", fe_rhs_text), env = env)
    fe_terms = attr(terms(fe_rhs), "term.labels")
    fe_vars = all.vars(fe_rhs)
    if (!length(fe_terms)) {
      stop("Fixed-effects block cannot be empty.")
    }
    if (length(fe_terms) != length(fe_vars) || !all(fe_terms %in% fe_vars)) {
      stop("Fixed effects must be specified as simple additive variable names.")
    }
    fe = unique(fe_terms)
  }

  list(
    fml = fml,
    env = env,
    yvar = yvar,
    exog_rhs = exog_rhs,
    exog_rhs_text = exog_rhs_text,
    exog_vars = all.vars(exog_rhs),
    endog_rhs = endog_rhs,
    endog_rhs_text = endog_rhs_text,
    endog_vars = all.vars(endog_rhs),
    instr_rhs = instr_rhs,
    instr_rhs_text = instr_rhs_text,
    instr_vars = all.vars(instr_rhs),
    fe = fe,
    fe_rhs_text = fe_rhs_text
  )
}

#' @keywords internal
#' Expands the right-hand side of the structural formula into SQL expressions for the design matrix, including fixed effects if specified.
dbivreg_expand_block = function(rhs_formula, conn, table, fe_vars = character()) {
  if (is.null(fe_vars)) {
    fe_vars = character()
  }
  sql_design = sql_model_matrix(
    rhs_formula,
    conn,
    table,
    expand = "all",
    fe_vars = fe_vars
  )
  if (anyDuplicated(sql_design$col_names) > 0) {
    dupes = unique(sql_design$col_names[duplicated(sql_design$col_names)])
    stop(
      "IV design matrix contains duplicate column names: ",
      paste(standardize_coef_names(dupes), collapse = ", ")
    )
  }
  list(
    sql = sql_design$select_exprs,
    names = sql_design$col_names,
    vars = all.vars(rhs_formula),
    term_labels = attr(terms(rhs_formula), "term.labels")
  )
}

#' @keywords internal
dbivreg_find_top_level = function(x, delim) {
  chars = strsplit(x, "", fixed = TRUE)[[1]]
  if (!length(chars)) {
    return(NA_integer_)
  }

  depth_paren = 0L
  depth_bracket = 0L
  depth_brace = 0L
  in_single = FALSE
  in_double = FALSE
  in_backtick = FALSE

  for (i in seq_along(chars)) {
    ch = chars[i]
    prev = if (i > 1L) chars[i - 1L] else ""

    if (in_single) {
      if (ch == "'" && prev != "\\") in_single = FALSE
      next
    }
    if (in_double) {
      if (ch == "\"" && prev != "\\") in_double = FALSE
      next
    }
    if (in_backtick) {
      if (ch == "`") in_backtick = FALSE
      next
    }

    if (ch == "'") {
      in_single = TRUE
      next
    }
    if (ch == "\"") {
      in_double = TRUE
      next
    }
    if (ch == "`") {
      in_backtick = TRUE
      next
    }

    if (ch == "(") {
      depth_paren = depth_paren + 1L
      next
    }
    if (ch == ")") {
      depth_paren = max(depth_paren - 1L, 0L)
      next
    }
    if (ch == "[") {
      depth_bracket = depth_bracket + 1L
      next
    }
    if (ch == "]") {
      depth_bracket = max(depth_bracket - 1L, 0L)
      next
    }
    if (ch == "{") {
      depth_brace = depth_brace + 1L
      next
    }
    if (ch == "}") {
      depth_brace = max(depth_brace - 1L, 0L)
      next
    }

    if (
      ch == delim &&
      depth_paren == 0L &&
      depth_bracket == 0L &&
      depth_brace == 0L
    ) {
      return(i)
    }
  }

  NA_integer_
}

#' @keywords internal
dbivreg_split_top_level = function(x, delim) {
  chars = strsplit(x, "", fixed = TRUE)[[1]]
  if (!length(chars)) {
    return(character(0))
  }

  starts = 1L
  parts = character(0)
  repeat {
    subx = paste(chars[starts:length(chars)], collapse = "")
    pos = dbivreg_find_top_level(subx, delim)
    if (is.na(pos)) {
      parts = c(parts, dbivreg_trim_ws(subx))
      break
    }
    end = starts + pos - 2L
    parts = c(parts, dbivreg_trim_ws(paste(chars[starts:end], collapse = "")))
    starts = starts + pos
  }
  parts
}

#' @keywords internal
dbivreg_trim_ws = function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#' @keywords internal
compute_iv_vcov = function(
  vcov_type = "iid",
  A_inv,
  ZtZ_inv,
  ZtX,
  rss,
  df_res,
  nobs_orig,
  n_params,
  meat = NULL
) {
  if (vcov_type == "iid") {
    sigma2 = rss / df_res
    vcov_mat = sigma2 * A_inv
    attr(vcov_mat, "type") = "iid"
  } else {
    if (is.null(meat)) {
      stop("Robust IV variance requires a meat matrix.")
    }
    middle = Matrix::t(ZtX) %*% ZtZ_inv %*% meat %*% ZtZ_inv %*% ZtX
    if (vcov_type == "hc1") {
      scale_hc1 = nobs_orig / df_res
      vcov_mat = scale_hc1 * (A_inv %*% middle %*% A_inv)
      attr(vcov_mat, "type") = "hc1"
    } else if (vcov_type == "cluster") {
      n_clusters = attr(meat, "n_clusters")
      if (is.null(n_clusters)) {
        stop("Clustered IV meat matrix missing n_clusters attribute.")
      }
      scale_cr1 = (n_clusters / (n_clusters - 1)) * ((nobs_orig - 1) / (nobs_orig - n_params))
      vcov_mat = scale_cr1 * (A_inv %*% middle %*% A_inv)
      attr(vcov_mat, "type") = "cluster"
      attr(vcov_mat, "n_clusters") = n_clusters
    } else {
      stop("Unsupported vcov type: ", vcov_type)
    }
  }
  dimnames(vcov_mat) = dimnames(A_inv)
  vcov_mat
}

#' @keywords internal
compute_dbivreg_diagnostics = function(
  inputs,
  moms,
  cte_sql,
  cte_name,
  has_intercept,
  weights_expr,
  n_total,
  df_fe,
  rss,
  betahat,
  S_zz,
  S_zx,
  S_zy,
  meat
) {
  overid = compute_dbivreg_overid(
    inputs = inputs,
    betahat = betahat,
    S_zz = S_zz,
    S_zx = S_zx,
    S_zy = S_zy,
    rss = rss,
    n_total = n_total,
    meat = meat
  )
  list(
    first_stage_f = compute_dbivreg_first_stage_f(
      inputs = inputs,
      moms = moms,
      n_total = n_total,
      df_fe = df_fe,
      has_intercept = has_intercept
    ),
    first_stage_wald = compute_dbivreg_first_stage_wald(
      inputs = inputs,
      moms = moms,
      cte_sql = cte_sql,
      cte_name = cte_name,
      has_intercept = has_intercept,
      weights_expr = weights_expr,
      n_total = n_total,
      df_fe = df_fe
    ),
    overid = overid,
    sargan = if (identical(overid$test, "Sargan")) overid else NULL,
    hansen_j = if (identical(overid$test, "Hansen J")) overid else NULL
  )
}

#' @keywords internal
compute_dbivreg_first_stage_f = function(inputs, moms, n_total, df_fe, has_intercept) {
  excluded = inputs[["instr_names"]]
  endogenous = inputs[["endog_names"]]
  if (!length(excluded) || !length(endogenous)) {
    return(NULL)
  }

  unrestricted = c(if (has_intercept) "(Intercept)", inputs[["exog_names"]], excluded)
  restricted = c(if (has_intercept) "(Intercept)", inputs[["exog_names"]])
  q = length(excluded)
  out = vector("list", length(endogenous))
  names(out) = standardize_coef_names(endogenous)

  for (j in seq_along(endogenous)) {
    y_name = endogenous[j]
    S_uu = moms$S_uu[unrestricted, unrestricted, drop = FALSE]
    S_uy = moms$S_uu[unrestricted, y_name, drop = FALSE]
    fit_u = solve_with_fallback(S_uu, S_uy)
    beta_u = fit_u$betahat
    yy = moms$S_uu[y_name, y_name]
    rss_u = as.numeric(yy - 2 * Matrix::t(beta_u) %*% S_uy + Matrix::t(beta_u) %*% S_uu %*% beta_u)

    if (length(restricted) == 0) {
      rss_r = yy
    } else {
      S_rr = moms$S_uu[restricted, restricted, drop = FALSE]
      S_ry = moms$S_uu[restricted, y_name, drop = FALSE]
      fit_r = solve_with_fallback(S_rr, S_ry)
      beta_r = fit_r$betahat
      rss_r = as.numeric(yy - 2 * Matrix::t(beta_r) %*% S_ry + Matrix::t(beta_r) %*% S_rr %*% beta_r)
    }

    df2 = max(n_total - length(unrestricted) - df_fe, 1)
    stat = ((rss_r - rss_u) / q) / (rss_u / df2)
    stat = max(as.numeric(stat), 0)
    out[[j]] = list(
      stat = stat,
      p = stats::pf(stat, q, df2, lower.tail = FALSE),
      df1 = q,
      df2 = df2
    )
  }

  out
}

#' @keywords internal
compute_dbivreg_first_stage_wald = function(
  inputs,
  moms,
  cte_sql,
  cte_name,
  has_intercept,
  weights_expr,
  n_total,
  df_fe
) {
  excluded = inputs[["instr_names"]]
  endogenous = inputs[["endog_names"]]
  if (!length(excluded) || !length(endogenous)) {
    return(NULL)
  }

  unrestricted = c(if (has_intercept) "(Intercept)", inputs[["exog_names"]], excluded)
  regressors = c(inputs[["exog_names"]], excluded)
  is_athena = inherits(inputs[["conn"]], "AthenaConnection")
  q = length(excluded)
  vcov_label = dbivreg_vcov_label(inputs[["vcov_type_req"]], NULL)
  out = vector("list", length(endogenous))
  names(out) = standardize_coef_names(endogenous)

  for (j in seq_along(endogenous)) {
    y_name = endogenous[j]
    S_uu = moms$S_uu[unrestricted, unrestricted, drop = FALSE]
    S_uy = moms$S_uu[unrestricted, y_name, drop = FALSE]
    fit_u = solve_with_fallback(S_uu, S_uy)
    beta_u = fit_u$betahat
    XtX_inv = fit_u$XtX_inv
    rownames(beta_u) = unrestricted

    yy = moms$S_uu[y_name, y_name]
    rss_u = as.numeric(yy - 2 * Matrix::t(beta_u) %*% S_uy + Matrix::t(beta_u) %*% S_uu %*% beta_u)
    df2 = max(n_total - length(unrestricted) - df_fe, 1)

    if (inputs[["vcov_type_req"]] == "iid") {
      vcov_u = (rss_u / df2) * XtX_inv
      attr(vcov_u, "type") = "iid"
    } else if (inputs[["vcov_type_req"]] == "hc1") {
      meat = compute_meat_sql(
        conn = inputs[["conn"]],
        cte_sql = cte_sql,
        vars = regressors,
        yvar = y_name,
        betahat = beta_u,
        is_athena = is_athena,
        var_suffix = if (has_intercept) "" else "_tilde",
        cte_name = cte_name,
        has_intercept = has_intercept,
        vars_sql = if (has_intercept) regressors else paste0(regressors, "_tilde"),
        weights_expr = weights_expr
      )
      vcov_u = compute_vcov(
        vcov_type = "hc1",
        strategy = if (has_intercept) "moments" else "demean",
        XtX_inv = XtX_inv,
        rss = rss_u,
        df_res = df2,
        nobs_orig = n_total,
        n_params = length(unrestricted) + df_fe,
        meat = meat
      )
      vcov_label = dbivreg_vcov_label("hc1", NULL)
    } else if (inputs[["vcov_type_req"]] == "cluster") {
      meat = compute_meat_cluster_sql(
        conn = inputs[["conn"]],
        cte_sql = cte_sql,
        vars = regressors,
        yvar = y_name,
        betahat = beta_u,
        cluster_var = inputs[["cluster_var"]],
        is_athena = is_athena,
        var_suffix = if (has_intercept) "" else "_tilde",
        cte_name = cte_name,
        has_intercept = has_intercept,
        vars_sql = if (has_intercept) regressors else paste0(regressors, "_tilde"),
        weights_expr = weights_expr
      )
      vcov_u = compute_vcov(
        vcov_type = "cluster",
        strategy = if (has_intercept) "moments" else "demean",
        XtX_inv = XtX_inv,
        rss = rss_u,
        df_res = df2,
        nobs_orig = n_total,
        n_params = length(unrestricted) + df_fe,
        meat = meat
      )
      n_clusters = attr(vcov_u, "n_clusters")
      vcov_u = vcov_u * ((n_total - 1) / n_total)
      attr(vcov_u, "type") = "cluster"
      attr(vcov_u, "n_clusters") = n_clusters
      vcov_label = dbivreg_vcov_label("cluster", attr(vcov_u, "n_clusters"))
    } else {
      stop("Unsupported vcov type for first-stage diagnostics: ", inputs[["vcov_type_req"]])
    }

    beta_q = beta_u[excluded, , drop = FALSE]
    vcov_q = vcov_u[excluded, excluded, drop = FALSE]
    inv_vcov_q = solve_with_fallback(vcov_q, beta_q)$betahat
    stat = as.numeric(Matrix::t(beta_q) %*% inv_vcov_q) / q
    stat = max(stat, 0)
    out[[j]] = list(
      stat = stat,
      p = stats::pf(stat, q, df2, lower.tail = FALSE),
      df1 = q,
      df2 = df2,
      vcov = vcov_label
    )
  }

  out
}

#' @keywords internal
compute_dbivreg_overid = function(inputs, betahat, S_zz, S_zx, S_zy, rss, n_total, meat = NULL) {
  df = length(inputs[["instr_names"]]) - length(inputs[["endog_names"]])
  if (df <= 0) {
    test_name = if (identical(inputs[["vcov_type_req"]], "iid")) "Sargan" else "Hansen J"
    return(list(stat = NA_real_, p = NA_real_, df = max(df, 0L), test = test_name))
  }

  moments = S_zy - S_zx %*% betahat
  if (identical(inputs[["vcov_type_req"]], "iid")) {
    z_inv = solve_with_fallback(S_zz, moments)$betahat
    stat = as.numeric(Matrix::t(moments) %*% z_inv) / (rss / n_total)
    test_name = "Sargan"
  } else {
    if (is.null(meat)) {
      return(list(stat = NA_real_, p = NA_real_, df = df, test = "Hansen J"))
    }
    omega_inv_m = solve_with_fallback(meat, moments)$betahat
    stat = as.numeric(Matrix::t(moments) %*% omega_inv_m)
    test_name = "Hansen J"
  }
  stat = max(stat, 0)

  list(
    stat = stat,
    p = stats::pchisq(stat, df = df, lower.tail = FALSE),
    df = df,
    test = test_name
  )
}

#' @keywords internal
dbivreg_vcov_label = function(vcov_type, n_clusters = NULL) {
  switch(
    vcov_type,
    "iid" = "IID",
    "hc1" = "Heteroskedasticity-robust",
    "cluster" = if (!is.null(n_clusters)) sprintf("Clustered (%d clusters)", n_clusters) else "Clustered",
    vcov_type
  )
}

#' @keywords internal
compute_iv_meat_sql = function(
  conn,
  cte_sql,
  x_vars,
  z_vars,
  yvar,
  betahat,
  weights_expr = NULL,
  cte_name = "base",
  has_intercept = TRUE,
  is_athena = FALSE,
  x_vars_sql = x_vars,
  z_vars_sql = z_vars,
  yvar_sql = yvar
) {
  beta_vals = as.numeric(betahat[c(if (has_intercept) "(Intercept)" else NULL, x_vars), 1])
  if (has_intercept) {
    intercept_val = beta_vals[1]
    beta_vals = beta_vals[-1]
  }

  weight_sql = if (is.null(weights_expr)) NULL else sprintf("CAST((%s) * (%s) AS FLOAT)", weights_expr, weights_expr)
  build_sum = function(parts, alias) {
    parts = parts[!vapply(parts, is.null, logical(1))]
    sprintf("SUM(%s) AS %s", paste(parts, collapse = " * "), alias)
  }

  beta_terms = if (length(x_vars_sql)) {
    paste(sprintf("%.15g * %s", beta_vals, x_vars_sql), collapse = " + ")
  } else {
    "0"
  }
  if (has_intercept) {
    resid_expr = sprintf("(%s - %.15g - (%s))", yvar_sql, intercept_val, beta_terms)
  } else {
    resid_expr = sprintf("(%s - (%s))", yvar_sql, beta_terms)
  }

  meat_terms = character(0)
  if (has_intercept) {
    meat_terms = c(meat_terms, build_sum(c(
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      weight_sql
    ), "meat_0_0"))
    for (j in seq_along(z_vars_sql)) {
      meat_terms = c(meat_terms, build_sum(c(
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", z_vars_sql[j]),
        weight_sql
      ), sprintf("meat_0_%d", j)))
    }
  }
  for (i in seq_along(z_vars_sql)) {
    for (j in i:length(z_vars_sql)) {
      meat_terms = c(meat_terms, build_sum(c(
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", resid_expr),
        sprintf("CAST(%s AS FLOAT)", z_vars_sql[i]),
        sprintf("CAST(%s AS FLOAT)", z_vars_sql[j]),
        weight_sql
      ), sprintf("meat_%d_%d", i, j)))
    }
  }

  meat_sql = paste0(
    cte_sql,
    ",\nmeat AS (SELECT ",
    paste(meat_terms, collapse = ", "),
    " FROM ",
    cte_name,
    ")\nSELECT * FROM meat"
  )
  if (is_athena) {
    meat_sql = gsub("FLOAT", "REAL", meat_sql, fixed = TRUE)
  }

  meat_df = dbGetQuery(conn, meat_sql)
  vars_all = if (has_intercept) c("(Intercept)", z_vars) else z_vars
  meat_mat = matrix(0, length(vars_all), length(vars_all), dimnames = list(vars_all, vars_all))

  if (has_intercept) {
    meat_mat[1, 1] = meat_df$meat_0_0
    for (j in seq_along(z_vars)) {
      val = meat_df[[sprintf("meat_0_%d", j)]]
      meat_mat[1, j + 1] = meat_mat[j + 1, 1] = val
    }
  }
  for (i in seq_along(z_vars)) {
    for (j in i:length(z_vars)) {
      val = meat_df[[sprintf("meat_%d_%d", i, j)]]
      idx_i = if (has_intercept) i + 1 else i
      idx_j = if (has_intercept) j + 1 else j
      meat_mat[idx_i, idx_j] = meat_mat[idx_j, idx_i] = val
    }
  }
  meat_mat
}

#' @keywords internal
compute_iv_meat_cluster_sql = function(
  conn,
  cte_sql,
  x_vars,
  z_vars,
  yvar,
  betahat,
  cluster_var,
  weights_expr = NULL,
  cte_name = "base",
  has_intercept = TRUE,
  is_athena = FALSE,
  x_vars_sql = x_vars,
  z_vars_sql = z_vars,
  yvar_sql = yvar
) {
  beta_vals = as.numeric(betahat[c(if (has_intercept) "(Intercept)" else NULL, x_vars), 1])
  if (has_intercept) {
    intercept_val = beta_vals[1]
    beta_vals = beta_vals[-1]
  }

  weight_sql = if (is.null(weights_expr)) NULL else sprintf("CAST(%s AS FLOAT)", weights_expr)
  build_sum = function(parts, alias) {
    parts = parts[!vapply(parts, is.null, logical(1))]
    sprintf("SUM(%s) AS %s", paste(parts, collapse = " * "), alias)
  }

  beta_terms = if (length(x_vars_sql)) {
    paste(sprintf("%.15g * %s", beta_vals, x_vars_sql), collapse = " + ")
  } else {
    "0"
  }
  if (has_intercept) {
    resid_expr = sprintf("(%s - %.15g - (%s))", yvar_sql, intercept_val, beta_terms)
  } else {
    resid_expr = sprintf("(%s - (%s))", yvar_sql, beta_terms)
  }

  score_terms = character(0)
  if (has_intercept) {
    score_terms = c(score_terms, build_sum(c(
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      weight_sql
    ), "score_0"))
  }
  for (j in seq_along(z_vars_sql)) {
    score_terms = c(score_terms, build_sum(c(
      sprintf("CAST(%s AS FLOAT)", resid_expr),
      sprintf("CAST(%s AS FLOAT)", z_vars_sql[j]),
      weight_sql
    ), sprintf("score_%d", j)))
  }

  cluster_sql = paste0(
    cte_sql,
    ",\ncluster_scores AS (\n  SELECT ",
    cluster_var,
    ", ",
    paste(score_terms, collapse = ", "),
    "\n  FROM ",
    cte_name,
    "\n  GROUP BY ",
    cluster_var,
    "\n)\nSELECT * FROM cluster_scores"
  )
  if (is_athena) {
    cluster_sql = gsub("FLOAT", "REAL", cluster_sql, fixed = TRUE)
  }

  cluster_df = dbGetQuery(conn, cluster_sql)
  vars_all = if (has_intercept) c("(Intercept)", z_vars) else z_vars
  meat_mat = matrix(0, length(vars_all), length(vars_all), dimnames = list(vars_all, vars_all))
  score_cols = if (has_intercept) c("score_0", paste0("score_", seq_along(z_vars))) else paste0("score_", seq_along(z_vars))

  for (i in seq_len(nrow(cluster_df))) {
    s_g = as.numeric(cluster_df[i, score_cols])
    meat_mat = meat_mat + tcrossprod(s_g)
  }
  attr(meat_mat, "n_clusters") = nrow(cluster_df)
  meat_mat
}

#' Print method for dbivreg objects
#'
#' @param x A `dbivreg` object.
#' @param ... Additional unused arguments.
#' @export
print.dbivreg = function(x, ...) {
  ct = x[["coeftable"]]
  colnames(ct) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  se_type = attr(x$vcov, "type")
  n_clusters = attr(x$vcov, "n_clusters")
  se_type = dbivreg_vcov_label(se_type, n_clusters)

  if (x$strategy == "moments") {
    cat("Moments-based 2SLS estimation, Dep. Var.:", x$yvar, "\n")
    cat("Observations.:", prettyNum(x$nobs_orig, big.mark = ","), "\n")
  } else if (x$strategy == "demean") {
    mstring = if (length(x$fe) == 1) {
      "Demeaned"
    } else if (length(x$fe) == 2) {
      "Double Demeaned"
    } else {
      "Multi-way Demeaned"
    }
    cat(mstring, "2SLS estimation, Dep. Var.:", x$yvar, "\n")
    cat("Observations.:", prettyNum(x$nobs_orig, big.mark = ","), "\n")
  }
  cat("Endogenous:", paste(x$endogenous, collapse = ", "), "\n")
  cat("Excluded instruments:", paste(x$instruments, collapse = ", "), "\n")
  cat("Standard Errors:", se_type, "\n")
  if (!is.null(x$diagnostics$first_stage_f)) {
    fst_lines = vapply(
      names(x$diagnostics$first_stage_f),
      function(nm) {
        diag = x$diagnostics$first_stage_f[[nm]]
        sprintf("%s = %.3f (p=%.3g)", nm, diag$stat, diag$p)
      },
      character(1)
    )
    cat("First-stage F:", paste(fst_lines, collapse = ", "), "\n")
  }
  if (!identical(se_type, "IID") && !is.null(x$diagnostics$first_stage_wald)) {
    wald_lines = vapply(
      names(x$diagnostics$first_stage_wald),
      function(nm) {
        diag = x$diagnostics$first_stage_wald[[nm]]
        sprintf("%s = %.3f (p=%.3g)", nm, diag$stat, diag$p)
      },
      character(1)
    )
    cat("First-stage Wald:", paste(wald_lines, collapse = ", "), "\n")
  }
  if (!is.null(x$diagnostics$overid) && is.finite(x$diagnostics$overid$stat)) {
    cat(
      paste0("Over-ID (", x$diagnostics$overid$test, "):"),
      sprintf("%.3f (p=%.3g, df=%d)", x$diagnostics$overid$stat, x$diagnostics$overid$p, x$diagnostics$overid$df),
      "\n"
    )
  }

  print_coeftable(ct, gof_vals = gof(x), has_fes = !is.null(x$fe))
  invisible(ct)
}
