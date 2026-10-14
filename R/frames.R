# Estimation frames
#
# A frame is the CTE that both the moments pass and the score pass read from.
# Each build_*_frame() returns a list with:
#   cte_sql, cte_name  CTE text and the name of its final table
#   cols               named character: variable name -> SQL column in cte_name
#   weights_expr       SQL weight expression in cte_name (NULL if unweighted)
#   intercept          whether the frame keeps a constant (FALSE once demeaned)
#   cast               whether moments CAST columns before aggregating
#   prefix_terms       count aggregates (n_total, n_fe1, ...)
#   cleanup            function dropping any temp tables the frame created

# SELECT list: id columns, the outcome, aliased regressors, then any extra
# (weights, cluster) columns not already selected
frame_select = function(y, x_sql, x_names, id_cols = NULL, extra = NULL) {
  c(
    id_cols,
    y,
    sprintf("%s AS %s", x_sql, x_names),
    setdiff(unique(extra), c(id_cols, y, x_names))
  )
}

#' Untransformed estimation frame
#'
#' Selects the outcome, regressors (aliased to `x_names`), weights and cluster
#' columns into a `base` CTE.
#'
#' @param inputs Inputs environment.
#' @param x_sql,x_names SQL expressions and column names for the regressors.
#' @keywords internal
build_base_frame = function(inputs, x_sql, x_names) {
  y = inputs[["yvar"]]
  sel = frame_select(y, x_sql, x_names, extra = c(inputs[["weights"]], inputs[["cluster_var"]]))
  list(
    cte_sql = paste0("WITH base AS (\n  SELECT ", paste(sel, collapse = ", "), " ", inputs[["from_statement"]], "\n)"),
    cte_name = "base",
    cols = setNames(c(y, x_names), c(y, x_names)),
    weights_expr = sql_weight_expr(inputs[["weights"]]),
    intercept = TRUE,
    cast = FALSE,
    prefix_terms = sql_count(inputs[["conn"]], "n_total"),
    cleanup = function() invisible(NULL)
  )
}

#' Demeaned (within) estimation frame
#'
#' Sweeps out the fixed effects from the outcome and regressors, producing
#' `<var>_tilde` columns in a `demeaned` CTE. One FE and balanced, unweighted
#' two-way panels are demeaned analytically in SQL. Otherwise (3+ FE, or two
#' FE that are weighted or unbalanced) alternating projections are run into a
#' temp table, which `cleanup()` drops.
#'
#' @inheritParams build_base_frame
#' @param tag Prefix for messages, e.g. `"dbreg"`.
#' @keywords internal
build_demean_frame = function(inputs, x_sql, x_names, tag = "dbreg") {
  conn = inputs[["conn"]]
  fe = inputs[["fe"]]
  y = inputs[["yvar"]]
  weights = inputs[["weights"]]
  cluster_var = inputs[["cluster_var"]]
  vars = c(y, x_names)
  w_base = sql_weight_expr(weights)

  use_ap = length(fe) > 2
  if (length(fe) == 2) {
    is_balanced = inputs[["is_balanced"]]
    if (is.null(is_balanced)) {
      is_balanced = dbreg_is_balanced_panel(conn, inputs[["from_statement"]], fe)
    }
    use_ap = !is.null(weights) || !isTRUE(is_balanced)
  }
  if (use_ap && inputs[["verbose"]]) {
    message(sprintf("[%s] Using alternating projections for FE demeaning", tag))
  }
  if (use_ap && inputs[["sql_only"]]) {
    stop(sprintf("[%s] sql_only is not supported for alternating projections.", tag), call. = FALSE)
  }

  cleanup = function() invisible(NULL)
  if (use_ap) {
    ap_res = dbreg_alternating_projections(
      conn = conn,
      from_statement = inputs[["from_statement"]],
      fe = fe,
      yvar = y,
      xvars_sql = x_sql,
      xvar_names = x_names,
      weights = weights,
      cluster_var = cluster_var,
      verbose = inputs[["verbose"]]
    )
    ap_tables = c(ap_res[["table"]], ap_res[["base_table"]])
    cleanup = function() {
      backend = detect_backend(conn)[["name"]]
      for (tbl in ap_tables) {
        drop_table_if_exists(conn, tbl, backend)
      }
    }
    cte_sql = paste0("WITH demeaned AS (SELECT * FROM ", ap_res[["table"]], ")")
    weights_expr = "__w"
  } else {
    # Analytic demeaning: subtract group means for each FE (and add back the
    # overall mean for two-way panels)
    means = function(alias, suffix, group = NULL) {
      cols = vapply(vars, function(v) sql_weighted_mean(v, w_base, paste0(v, suffix)), character(1))
      sprintf(
        "%s AS (SELECT %s FROM base%s)",
        alias,
        paste(c(group, cols), collapse = ", "),
        if (is.null(group)) "" else paste0(" GROUP BY ", group)
      )
    }
    if (length(fe) == 1) {
      mean_ctes = means("group_means", "_mean", fe[1])
      tilde = sprintf("(b.%s - gm.%s_mean) AS %s_tilde", vars, vars, vars)
      joins = sprintf("JOIN group_means gm ON b.%s = gm.%s", fe[1], fe[1])
    } else {
      mean_ctes = c(
        means("unit_means", "_u", fe[1]),
        means("time_means", "_t", fe[2]),
        means("overall", "_o")
      )
      tilde = sprintf("(b.%s - um.%s_u - tm.%s_t + o.%s_o) AS %s_tilde", vars, vars, vars, vars, vars)
      joins = c(
        sprintf("JOIN unit_means um ON b.%s = um.%s", fe[1], fe[1]),
        sprintf("JOIN time_means tm ON b.%s = tm.%s", fe[2], fe[2]),
        "CROSS JOIN overall o"
      )
    }
    passthrough = c(
      if (!is.null(weights)) sprintf("b.%s AS weights", weights),
      if (!is.null(cluster_var) && !cluster_var %in% fe) sprintf("b.%s AS %s", cluster_var, cluster_var)
    )
    sel = frame_select(y, x_sql, x_names, id_cols = fe, extra = c(weights, cluster_var))
    cte_sql = paste0(
      "WITH base AS (\n  SELECT ", paste(sel, collapse = ", "), " ", inputs[["from_statement"]], "\n),\n",
      paste(mean_ctes, collapse = ",\n"), ",\n",
      "demeaned AS (\n  SELECT ", paste(c(paste0("b.", fe), tilde, passthrough), collapse = ",\n    "),
      "\n  FROM base b\n  ", paste(joins, collapse = "\n  "), "\n)"
    )
    weights_expr = if (is.null(weights)) NULL else sql_weight_expr("weights")
  }

  list(
    cte_sql = cte_sql,
    cte_name = "demeaned",
    cols = setNames(paste0(vars, "_tilde"), vars),
    weights_expr = weights_expr,
    intercept = FALSE,
    cast = TRUE,
    prefix_terms = c(
      sql_count(conn, "n_total"),
      vapply(seq_along(fe), function(k) sql_count(conn, sprintf("n_fe%d", k), fe[k], distinct = TRUE), character(1))
    ),
    cleanup = cleanup
  )
}

#' Moments query over an estimation frame
#'
#' Appends the cross-moment aggregates for `y` and `x` (names in `frame$cols`)
#' to the frame's CTE. Read the result back with [cross_moments()].
#'
#' @param frame Result of a `build_*_frame()` function.
#' @param y,x Outcome and regressor names.
#' @param conn Database connection (for the Athena FLOAT gotcha).
#' @keywords internal
frame_moments_sql = function(frame, y, x, conn) {
  col = function(v) {
    sql = unname(frame[["cols"]][v])
    if (frame[["cast"]]) sprintf("CAST(%s AS FLOAT)", sql) else sql
  }
  terms = build_weighted_moment_terms(
    y_sql = col(y),
    x_sql = col(x),
    weights_expr = frame[["weights_expr"]],
    intercept = frame[["intercept"]],
    prefix_terms = frame[["prefix_terms"]]
  )
  sql = paste0(
    frame[["cte_sql"]], ",\nmoments AS (\n  SELECT\n    ",
    paste(terms, collapse = ",\n    "),
    "\n  FROM ", frame[["cte_name"]], "\n)\nSELECT * FROM moments"
  )
  # Athena FLOAT gotcha
  # https://github.com/DyfanJones/noctua/issues/228
  if (inherits(conn, "AthenaConnection")) {
    sql = gsub("FLOAT", "REAL", sql, fixed = TRUE)
  }
  sql
}
