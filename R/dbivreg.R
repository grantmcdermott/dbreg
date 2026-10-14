#' Instrumental-variables regression on a database backend
#'
#' @md
#' @description
#' Estimates a linear IV model via exact sufficient statistics on a database
#' backend. The current implementation supports three exact strategies:
#'
#' - `strategy = "moments"` for models without absorbed fixed effects.
#' - `strategy = "compress"` for repeated no-FE design rows.
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
#'   `"compress"`, and `"mundlak"`. The latter currently errors because it is
#'   not yet implemented for IV.
#' @param cluster Optional clustering variable, supplied as a one-sided formula
#'   or character string.
#' @param ssc Character string controlling the small-sample correction for
#'   clustered standard errors. Options are `"full"` (default) or `"nested"`.
#'   With `"nested"`, fixed effects nested within the cluster variable are
#'   excluded from K, matching the default behavior of `fixest::feols`. See
#'   [dbreg()] for details.
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
  ssc = c("full", "nested"),
  sql_only = FALSE,
  data_only = FALSE,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  verbose = isTRUE(verbose)
  strategy = match.arg(strategy)
  ssc = match.arg(ssc)
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
    ssc = ssc,
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
    "compress" = execute_iv_compress_strategy(inputs),
    "demean" = execute_iv_demean_strategy(inputs),
    stop("Unknown IV strategy: ", chosen_strategy)
  )

  finalize_dbreg_result(result, inputs, chosen_strategy, class = c("dbivreg", "dbreg"))
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
  ssc,
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

  validate_weights(weights, data, conn, from_statement)
  from_statement = filter_estimation_sample(
    from_statement,
    vars = c(
      yvar,
      parsed_fml[["exog_vars"]],
      parsed_fml[["endog_vars"]],
      parsed_fml[["instr_vars"]],
      fe,
      cluster_var
    ),
    weights = weights,
    drop_missings = drop_missings
  )

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
    ssc = ssc,
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

  if (chosen_strategy == "mundlak") {
    stop(
      "dbivreg() does not yet implement strategy = '",
      chosen_strategy,
      "'. Supported IV strategies are 'moments', 'compress', and 'demean'."
    )
  }
  if (chosen_strategy == "compress" && length(fe) > 0) {
    stop(
      "strategy = 'compress' is only available without fixed effects in dbivreg(). ",
      "Use strategy = 'demean' or strategy = 'auto' when fixed effects are present."
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
  frame = build_base_frame(inputs, inputs[["union_sql"]], inputs[["union_names"]])
  dbivreg_run_frame(inputs, frame, "IV moments")
}

#' @keywords internal
execute_iv_demean_strategy = function(inputs) {
  frame = build_demean_frame(inputs, inputs[["union_sql"]], inputs[["union_names"]], tag = "dbivreg")
  on.exit(frame[["cleanup"]](), add = TRUE)
  dbivreg_run_frame(inputs, frame, "demeaned IV")
}

# Moments pass over a frame, then the 2SLS fit with SQL score-pass meat
#' @keywords internal
dbivreg_run_frame = function(inputs, frame, label) {
  moments_sql = frame_moments_sql(frame, inputs[["yvar"]], inputs[["union_names"]], inputs[["conn"]])
  if (inputs[["sql_only"]]) {
    return(moments_sql)
  }
  if (inputs[["verbose"]]) {
    message(sprintf("[dbivreg] Executing %s%s SQL\n", if (!is.null(inputs[["weights"]])) "weighted " else "", label))
  }
  moments_df = dbGetQuery(inputs[["conn"]], moments_sql)
  if (inputs[["data_only"]]) {
    return(moments_df)
  }
  fe = inputs[["fe"]]
  dbivreg_finalize_fit(
    inputs = inputs,
    S = cross_moments(moments_df, inputs[["union_names"]], inputs[["yvar"]], intercept = frame[["intercept"]]),
    n_total = moments_df[["n_total"]],
    n_fe_levels = setNames(
      vapply(seq_along(fe), function(k) as.numeric(moments_df[[sprintf("n_fe%d", k)]]), numeric(1)),
      fe
    ),
    meats_fn = function(fits, ys) fit_meats(fits, frame, ys, inputs),
    query_string = moments_sql
  )
}

#' @keywords internal
execute_iv_compress_strategy = function(inputs) {
  if (length(inputs[["fe"]]) > 0) {
    stop(
      "strategy = 'compress' is only available without fixed effects in dbivreg(). ",
      "Use strategy = 'demean' or strategy = 'auto' when fixed effects are present."
    )
  }

  conn = inputs[["conn"]]
  yvar = inputs[["yvar"]]
  union_names = inputs[["union_names"]]
  weights = inputs[["weights"]]
  frame = build_base_frame(inputs, inputs[["union_sql"]], union_names)

  # Compress on all of X and Z: each cell is constant in both
  group_cols_sql = paste(union_names, collapse = ", ")
  moment_terms = build_weighted_moment_terms(
    y_sql = yvar,
    weights_expr = frame[["weights_expr"]],
    prefix_terms = "COUNT(*) AS n",
    include_w_sq = TRUE
  )
  compress_sql = paste0(
    frame[["cte_sql"]],
    "\nSELECT ", group_cols_sql, ",\n  ", paste(moment_terms, collapse = ",\n  "),
    "\nFROM base\nGROUP BY ", group_cols_sql
  )
  if (inputs[["sql_only"]]) {
    return(compress_sql)
  }
  if (inputs[["verbose"]]) {
    message(if (!is.null(weights)) "[dbivreg] Executing weighted compressed IV SQL\n" else "[dbivreg] Executing compressed IV SQL\n")
  }
  dat = dbGetQuery(conn, compress_sql)
  if (inputs[["data_only"]]) {
    return(dat)
  }

  n_total = sum(dat[["n"]])
  compression_ratio = nrow(dat) / max(n_total, 1)
  if (inputs[["verbose"]] && compression_ratio > 0.8) {
    warning(paste0(
      sprintf("[dbivreg] compression ineffective (%.1f%% of original rows). ", 100 * compression_ratio),
      "Consider strategy = 'moments'."
    ))
  }

  # Cell-level design over the union of X and Z, and the cross moments
  # U'WU, U'Wy, y'Wy (exact, since U is constant within cells)
  U = cbind("(Intercept)" = 1, as.matrix(dat[, union_names, drop = FALSE]))
  S = rbind(
    cbind(crossprod(U, dat[["sum_w"]] * U), crossprod(U, dat[["sum_wy"]])),
    c(crossprod(dat[["sum_wy"]], U), sum(dat[["sum_wy_sq"]]))
  )
  dimnames(S) = list(c(colnames(U), yvar), c(colnames(U), yvar))

  # Meat from the cells: HC1 from within-cell residual sums of squares;
  # clustered from cluster-by-cell residual sums (one query, shared by the
  # 2SLS fit and its first stages, whose outcomes are cell columns)
  vcov_type = inputs[["vcov_type_req"]]
  cells = if (vcov_type == "cluster") {
    query_cluster_cells(
      conn = conn,
      from_statement = paste0("FROM (", frame[["base_sql"]], ") AS base"),
      group_cols = union_names,
      yvar = yvar,
      cluster_var = inputs[["cluster_var"]],
      weights = weights
    )
  }
  meats_fn = function(fits, ys) {
    if (!vcov_type %in% c("hc1", "cluster")) {
      return(vector("list", length(fits)))
    }
    Map(function(fit, y) {
      Z = U[, fit[["scores"]], drop = FALSE]
      yhat = as.numeric(U[, fit[["x"]], drop = FALSE] %*% fit[["coef"]])
      if (vcov_type == "cluster") {
        return(cluster_meat_from_cells(
          cells, dat, union_names, inputs[["cluster_var"]], Z, yhat,
          y_cell = if (y != yvar) dat[[y]]
        ))
      }
      # Sum of (squared) weights times y and y^2 per cell; a first-stage
      # outcome is itself a cell column
      w = if (is.null(weights)) dat[["sum_w"]] else dat[["sum_w2"]]
      if (y == yvar) {
        wy = if (is.null(weights)) dat[["sum_wy"]] else dat[["sum_w2y"]]
        wy_sq = if (is.null(weights)) dat[["sum_wy_sq"]] else dat[["sum_w2y_sq"]]
      } else {
        wy = w * dat[[y]]
        wy_sq = w * dat[[y]]^2
      }
      crossprod(Z, (wy_sq - 2 * yhat * wy + w * yhat^2) * Z)
    }, fits, ys)
  }

  result = dbivreg_finalize_fit(
    inputs = inputs,
    S = S,
    n_total = n_total,
    n_fe_levels = numeric(0),
    meats_fn = meats_fn,
    query_string = compress_sql
  )
  result[["nobs"]] = nrow(dat)
  result[["compression_ratio"]] = compression_ratio
  result
}

#' @keywords internal
dbivreg_finalize_fit = function(inputs, S, n_total, n_fe_levels, meats_fn, query_string) {
  yvar = inputs[["yvar"]]
  has_intercept = "(Intercept)" %in% rownames(S)
  x_names_full = c(if (has_intercept) "(Intercept)", inputs[["x_all_names"]])
  z_names_full = c(if (has_intercept) "(Intercept)", inputs[["z_all_names"]])
  fit = fit_2sls(S, yvar, x_names_full, z_names_full)
  betahat = fit[["coef"]]
  rss = fit[["rss"]]

  n_fe = length(n_fe_levels)
  df_fe = if (n_fe == 0) 0 else sum(n_fe_levels) - (n_fe - 1)
  df_res = max(n_total - length(x_names_full) - df_fe, 1)
  # For ssc = "nested", exclude nested FE parameters from the CR1 K
  nested_levels = 0L
  if (inputs[["vcov_type_req"]] == "cluster" && inputs[["ssc"]] == "nested") {
    nested_levels = nested_fe_df(
      inputs[["conn"]], inputs[["from_statement"]], inputs[["fe"]], inputs[["cluster_var"]]
    )
  }
  n_params = length(x_names_full) + df_fe - nested_levels

  tss = moment_tss(S, yvar)

  # First stages: each endogenous regressor on the instruments
  endog = if (length(inputs[["instr_names"]])) inputs[["endog_names"]] else character(0)
  fs_fits = lapply(endog, function(y) fit_ols(S, y, z_names_full))

  # Score pass: residuals from the structural regressors (and the first
  # stages), all scored on the instruments in one pass; fit_2sls() projects
  # the structural meat into coefficient space
  meats = meats_fn(c(list(fit), fs_fits), c(yvar, endog))
  meat = meats[[1]]
  vcov_mat = compute_vcov(
    vcov_type = inputs[["vcov_type_req"]],
    strategy = if (has_intercept) "moments" else "demean",
    XtX_inv = fit[["bread"]],
    rss = rss,
    df_res = df_res,
    nobs_orig = n_total,
    n_params = n_params,
    meat = fit[["project_meat"]](meat)
  )
  attr(vcov_mat, "rss") = rss
  attr(vcov_mat, "tss") = tss

  coeftable = gen_coeftable(betahat, vcov_mat, df_res)
  iv_diagnostics = compute_dbivreg_diagnostics(
    inputs = inputs,
    S = S,
    fs_fits = fs_fits,
    fs_meats = meats[-1],
    fit = fit,
    z = z_names_full,
    has_intercept = has_intercept,
    n_total = n_total,
    df_fe = df_fe,
    meat = meat,
    nested_levels = nested_levels
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
    nobs_orig = n_total,
    estimator = "2SLS",
    endogenous = inputs[["endog_vars"]],
    instruments = inputs[["instr_vars"]],
    diagnostics = iv_diagnostics,
    df_residual = df_res,
    n_fe_levels = if (n_fe > 0) n_fe_levels else NULL
  )
  # Per-FE counts by position (n_fe1, n_fe2, ...)
  for (k in seq_len(n_fe)) {
    out[[sprintf("n_fe%d", k)]] = unname(n_fe_levels[k])
  }
  out
}

#' @keywords internal
dbivreg_structural_formula = function(yvar, exog_rhs, endog_rhs, fe_rhs = NULL, env = parent.frame()) {
  rhs = paste(exog_rhs, "+", endog_rhs)
  if (!is.null(fe_rhs)) {
    rhs = paste(rhs, "|", fe_rhs)
  }
  Formula(stats::as.formula(paste(yvar, "~", rhs), env = env))
}

#' Parse a fixest-style IV formula
#'
#' Walks the call tree. Since `~` is left-associative, `y ~ x | fe | d ~ z`
#' parses as `(y ~ x | fe | d) ~ z`: the outer right-hand side holds the
#' instruments, and the inner right-hand side splits on top-level `|` into
#' the exogenous, (optional) fixed-effects and endogenous blocks.
#'
#' @keywords internal
dbivreg_parse_formula = function(fml) {
  if (inherits(fml, "Formula")) {
    fml = formula(fml)
  }
  if (!inherits(fml, "formula")) {
    stop("`fml` must be a formula.")
  }
  usage = "`y ~ x | d ~ z` or `y ~ x | fe1 + fe2 | d ~ z`"
  is_op = function(e, op) is.call(e) && identical(e[[1]], as.name(op)) && length(e) == 3L
  if (length(fml) != 3L || !is_op(fml[[2]], "~")) {
    stop("dbivreg() requires a fixest-style IV formula: ", usage, ".")
  }

  env = environment(fml)
  inner = fml[[2]]
  instr_expr = fml[[3]]
  if (is_op(instr_expr, "|")) {
    stop("The IV block must be the final pipe-separated block: ", usage, ".")
  }
  split_pipes = function(e) {
    if (is_op(e, "|")) c(split_pipes(e[[2]]), list(e[[3]])) else list(e)
  }
  blocks = split_pipes(inner[[3]])
  if (!(length(blocks) %in% c(2L, 3L))) {
    stop("dbivreg() requires a fixest-style IV formula with the IV block last: ", usage, ".")
  }

  text = function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")
  rhs = function(e) stats::as.formula(call("~", e), env = env)

  yvar = all.vars(inner[[2]])
  if (length(yvar) != 1L) {
    stop("Exactly one outcome variable required.")
  }
  exog_expr = blocks[[1]]
  endog_expr = blocks[[length(blocks)]]

  fe = NULL
  fe_rhs_text = NULL
  if (length(blocks) == 3L) {
    fe_rhs = rhs(blocks[[2]])
    fe_terms = attr(terms(fe_rhs), "term.labels")
    fe_vars = all.vars(fe_rhs)
    if (!length(fe_terms)) {
      stop("Fixed-effects block cannot be empty.")
    }
    if (length(fe_terms) != length(fe_vars) || !all(fe_terms %in% fe_vars)) {
      stop("Fixed effects must be specified as simple additive variable names.")
    }
    fe = unique(fe_terms)
    fe_rhs_text = text(blocks[[2]])
  }

  list(
    fml = fml,
    env = env,
    yvar = yvar,
    exog_rhs = rhs(exog_expr),
    exog_rhs_text = text(exog_expr),
    exog_vars = all.vars(exog_expr),
    endog_rhs = rhs(endog_expr),
    endog_rhs_text = text(endog_expr),
    endog_vars = all.vars(endog_expr),
    instr_rhs = rhs(instr_expr),
    instr_rhs_text = text(instr_expr),
    instr_vars = all.vars(instr_expr),
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
compute_dbivreg_diagnostics = function(
  inputs,
  S,
  fs_fits,
  fs_meats,
  fit,
  z,
  has_intercept,
  n_total,
  df_fe,
  meat,
  nested_levels = 0L
) {
  overid = compute_dbivreg_overid(inputs, S, fit, z, n_total, meat)
  list(
    first_stage_f = compute_dbivreg_first_stage_f(inputs, S, n_total, df_fe, has_intercept),
    first_stage_wald = compute_dbivreg_first_stage_wald(
      inputs = inputs,
      fs_fits = fs_fits,
      fs_meats = fs_meats,
      has_intercept = has_intercept,
      n_total = n_total,
      df_fe = df_fe,
      nested_levels = nested_levels
    ),
    overid = overid,
    sargan = if (identical(overid$test, "Sargan")) overid else NULL,
    hansen_j = if (identical(overid$test, "Hansen J")) overid else NULL
  )
}

#' @keywords internal
compute_dbivreg_first_stage_f = function(inputs, S, n_total, df_fe, has_intercept) {
  excluded = inputs[["instr_names"]]
  endogenous = inputs[["endog_names"]]
  if (!length(excluded) || !length(endogenous)) {
    return(NULL)
  }

  unrestricted = c(if (has_intercept) "(Intercept)", inputs[["exog_names"]], excluded)
  restricted = c(if (has_intercept) "(Intercept)", inputs[["exog_names"]])
  q = length(excluded)
  df2 = max(n_total - length(unrestricted) - df_fe, 1)
  out = lapply(endogenous, function(y_name) {
    rss_u = fit_ols(S, y_name, unrestricted)[["rss"]]
    rss_r = if (length(restricted)) fit_ols(S, y_name, restricted)[["rss"]] else S[y_name, y_name]
    stat = max(((rss_r - rss_u) / q) / (rss_u / df2), 0)
    list(stat = stat, p = stats::pf(stat, q, df2, lower.tail = FALSE), df1 = q, df2 = df2)
  })
  names(out) = standardize_coef_names(endogenous)
  out
}

#' @keywords internal
compute_dbivreg_first_stage_wald = function(
  inputs,
  fs_fits,
  fs_meats,
  has_intercept,
  n_total,
  df_fe,
  nested_levels = 0L
) {
  excluded = inputs[["instr_names"]]
  endogenous = inputs[["endog_names"]]
  if (!length(excluded) || !length(endogenous)) {
    return(NULL)
  }

  unrestricted = c(if (has_intercept) "(Intercept)", inputs[["exog_names"]], excluded)
  q = length(excluded)
  df2 = max(n_total - length(unrestricted) - df_fe, 1)
  vcov_type = inputs[["vcov_type_req"]]
  out = Map(function(fit_u, meat_u) {
    vcov_u = compute_vcov(
      vcov_type = vcov_type,
      strategy = if (has_intercept) "moments" else "demean",
      XtX_inv = fit_u[["bread"]],
      rss = fit_u[["rss"]],
      df_res = df2,
      nobs_orig = n_total,
      n_params = length(unrestricted) + df_fe - if (vcov_type == "cluster") nested_levels else 0L,
      meat = meat_u
    )
    beta_q = fit_u[["coef"]][excluded, , drop = FALSE]
    vcov_q = vcov_u[excluded, excluded, drop = FALSE]
    stat = as.numeric(Matrix::t(beta_q) %*% solve_with_fallback(vcov_q, beta_q)$betahat) / q
    stat = max(stat, 0)
    list(
      stat = stat,
      p = stats::pf(stat, q, df2, lower.tail = FALSE),
      df1 = q,
      df2 = df2,
      vcov = format_vcov_label(vcov_type, attr(vcov_u, "n_clusters"))
    )
  }, fs_fits, fs_meats)
  names(out) = standardize_coef_names(endogenous)
  out
}

#' @keywords internal
compute_dbivreg_overid = function(inputs, S, fit, z, n_total, meat = NULL) {
  df = length(inputs[["instr_names"]]) - length(inputs[["endog_names"]])
  if (df <= 0) {
    test_name = if (identical(inputs[["vcov_type_req"]], "iid")) "Sargan" else "Hansen J"
    return(list(stat = NA_real_, p = NA_real_, df = max(df, 0L), test = test_name))
  }

  # Sample moment conditions Z'e
  moments = S[z, inputs[["yvar"]], drop = FALSE] - S[z, fit[["x"]], drop = FALSE] %*% fit[["coef"]]
  if (identical(inputs[["vcov_type_req"]], "iid")) {
    z_inv = solve_with_fallback(S[z, z, drop = FALSE], moments)$betahat
    stat = as.numeric(Matrix::t(moments) %*% z_inv) / (fit[["rss"]] / n_total)
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

#' Print method for `dbivreg` objects
#'
#' @md
#' @param x A `dbivreg` object.
#' @param ... Additional unused arguments.
#' @export
print.dbivreg = function(x, ...) {
  ct = x[["coeftable"]]
  colnames(ct) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  se_type = attr(x$vcov, "type")
  n_clusters = attr(x$vcov, "n_clusters")
  se_type = format_vcov_label(se_type, n_clusters)

  if (x$strategy == "compress") {
    cat("Compressed 2SLS estimation, Dep. Var.:", x$yvar, "\n")
    cat(
      "Observations.:",
      prettyNum(x$nobs_orig, big.mark = ","),
      "(original) |",
      prettyNum(x$nobs, big.mark = ","),
      "(compressed)",
      "\n"
    )
  } else if (x$strategy == "moments") {
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
