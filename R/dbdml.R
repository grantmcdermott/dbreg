#' Partially linear estimation on database backends
#'
#' @md
#' @description
#' Unified interface for partially linear estimation with two internal engines:
#'
#' - `method = "strata"`: A DuckDML-style leave-one-out (LOO) partialling-out
#'   estimator for discrete controls, computed from compressed group statistics.
#' - `method = "plm"`: A cross-fitted partially linear model (PLM) estimator.
#'
#' With `method = "auto"` (default), `dbdml` chooses the engine based on the
#' control variables and inference request.
#'
#' @param fml A formula with one outcome on the left-hand side. All variables
#'   on the right-hand side are candidate treatments or controls.
#' @param treat Character vector naming treatment variable(s). If `NULL`, the
#'   first variable on the first RHS part is used.
#' @param method Character string choosing the estimator:
#'   `"auto"` (default), `"strata"`, or `"plm"`.
#' @param conn Database connection. If `NULL`, an ephemeral DuckDB connection is
#'   created and closed on exit.
#' @param table,data,path Mutually exclusive data source arguments with
#'   precedence `table > data > path`.
#' @param vcov Character string or formula. Character options are `"iid"`,
#'   `"hc1"`, `"none"`, and `"bootstrap"`.
#' @param cluster Optional cluster variable (formula or character). If supplied,
#'   clustered inference is used in the PLM second stage.
#' @param folds Number of folds for `method = "plm"`. Default is 5.
#' @param ridge Ridge penalty used in PLM nuisance fits. Either a single
#'   non-negative scalar or a list like `list(y = 1, d = 1)`.
#' @param n_bootstraps Number of group bootstrap replications for
#'   `vcov = "bootstrap"` in `method = "strata"`.
#' @param seed Integer random seed used by bootstrap.
#' @param sql_only,data_only Logical. Supported only for `method = "strata"`.
#' @param drop_missings Logical indicating whether rows with missing values in
#'   required variables should be dropped.
#' @param verbose Logical indicating whether progress messages are printed.
#' @param ... Additional unused arguments.
#'
#' @return A list of class `"dbdml"`.
#' @export
#' @importFrom Formula Formula
#' @importFrom stats formula as.formula reformulate qt
#' @importFrom DBI dbGetQuery dbIsValid dbDisconnect
#' @importFrom duckdb duckdb_unregister
#' @importFrom glue glue
#' @examples
#' set.seed(1)
#' n = 400
#' g = sample(1:20, n, TRUE)
#' d = rnorm(n)
#' y = 1 + 2 * d + rnorm(20)[g] + rnorm(n)
#' df = data.frame(y = y, d = d, g = factor(g))
#'
#' # auto -> strata engine (discrete control g)
#' dbdml(y ~ d + g, data = df, treat = "d")
#'
#' # explicit PLM engine
#' x = rnorm(n)
#' d = 0.5 * x + rnorm(n)
#' y = 1.5 * d + 0.8 * x + rnorm(n)
#' df2 = data.frame(y = y, d = d, x = x)
#' dbdml(y ~ d + x, data = df2, method = "plm", treat = "d")
dbdml = function(
  fml,
  treat = NULL,
  method = c("auto", "strata", "plm"),
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  vcov = c("hc1", "iid", "none", "bootstrap"),
  cluster = NULL,
  folds = 5,
  ridge = 1,
  n_bootstraps = 0,
  seed = 42,
  sql_only = FALSE,
  data_only = FALSE,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
) {
  verbose = isTRUE(verbose)
  method = match.arg(method)

  if (!is.numeric(folds) || length(folds) != 1 || is.na(folds) || folds < 2) {
    stop("`folds` must be an integer >= 2.")
  }
  folds = as.integer(folds)

  if (!is.numeric(n_bootstraps) || length(n_bootstraps) != 1 || is.na(n_bootstraps) || n_bootstraps < 0) {
    stop("`n_bootstraps` must be a non-negative numeric scalar.")
  }
  n_bootstraps = as.integer(n_bootstraps)

  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("`seed` must be a numeric scalar.")
  }
  seed = as.integer(seed)

  # Parse vcov / cluster
  cluster_var = NULL
  if (inherits(vcov, "formula")) {
    cluster_vars = all.vars(vcov)
    if (length(cluster_vars) != 1) {
      stop("Only single-variable clustering is currently supported.")
    }
    cluster_var = cluster_vars[1]
    vcov = "cluster"
  } else if (is.character(vcov)) {
    vcov = tolower(vcov[1])
    vcov = match.arg(vcov, c("iid", "hc1", "none", "bootstrap"))
  } else {
    stop("`vcov` must be a character string or one-sided formula.")
  }

  if (!is.null(cluster)) {
    if (inherits(cluster, "formula")) {
      cluster_vars = all.vars(cluster)
      if (length(cluster_vars) != 1) {
        stop("Only single-variable clustering is currently supported.")
      }
      cluster_var = cluster_vars[1]
    } else if (is.character(cluster)) {
      cluster_var = cluster[1]
    } else {
      stop("`cluster` must be a formula (e.g. ~id) or character string.")
    }
    vcov = "cluster"
  }

  # Parse ridge nuisance settings
  ridge_parsed = parse_dbdml_ridge(ridge)

  # Parse formula and treatment/controls
  spec = parse_dbdml_spec(fml, treat)
  yvar = spec$yvar
  treatments = spec$treatments
  controls = spec$controls

  validate_simple_names(c(yvar, treatments, controls, cluster_var))

  # Setup database connection
  db_setup = setup_db_connection(conn, table, data, path, caller = "dbdml")
  conn = db_setup$conn
  own_conn = db_setup$own_conn
  from_statement = db_setup$from_statement
  registered_table = db_setup$registered_table

  cleanup_conn = function() {
    if (own_conn && dbIsValid(conn)) {
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

  # Choose engine
  if (method == "auto") {
    method = choose_dbdml_method(
      conn = conn,
      from_statement = from_statement,
      controls = controls,
      data = data,
      vcov = vcov,
      sql_only = sql_only,
      data_only = data_only
    )
    if (verbose) {
      message("[dbdml] auto-selected method: ", method)
    }
  }

  if ((sql_only || data_only) && method == "plm") {
    stop("`sql_only` and `data_only` are currently supported only for `method = 'strata'`.")
  }

  if (method == "strata") {
    if (vcov == "cluster") {
      stop("Clustered inference is not available for `method = 'strata'`.")
    }
    if (n_bootstraps > 0 && vcov != "bootstrap") {
      warning("[dbdml] n_bootstraps > 0: using bootstrap variance.")
      vcov = "bootstrap"
    }
    if (vcov == "bootstrap" && n_bootstraps == 0) {
      n_bootstraps = 200L
      warning("[dbdml] vcov = 'bootstrap' with n_bootstraps = 0. Using n_bootstraps = 200.")
    }

    result = dbdml_fit_strata(
      yvar = yvar,
      treatments = treatments,
      strata = controls,
      conn = conn,
      from_statement = from_statement,
      vcov = vcov,
      n_bootstraps = n_bootstraps,
      seed = seed,
      sql_only = sql_only,
      data_only = data_only,
      drop_missings = drop_missings,
      verbose = verbose
    )
  } else {
    result = dbdml_fit_plm(
      yvar = yvar,
      treatments = treatments,
      controls = controls,
      conn = conn,
      from_statement = from_statement,
      vcov = vcov,
      cluster_var = cluster_var,
      folds = folds,
      ridge_y = ridge_parsed$y,
      ridge_d = ridge_parsed$d,
      drop_missings = drop_missings,
      verbose = verbose
    )
  }

  if (isTRUE(sql_only) || isTRUE(data_only)) {
    return(result)
  }

  result$method = method
  class(result) = c("dbdml", class(result))
  result
}

#' @keywords internal
parse_dbdml_spec = function(fml, treat = NULL) {
  fml = Formula(fml)
  if (length(fml)[2] > 2) {
    stop("dbdml supports at most one `|` in the formula.")
  }

  yvar = all.vars(formula(fml, lhs = 1, rhs = 0))
  if (length(yvar) != 1) {
    stop("Exactly one outcome variable is required.")
  }

  rhs1 = formula(fml, lhs = 0, rhs = 1)
  term_labels1 = attr(terms(rhs1), "term.labels")
  rhs1_vars = all.vars(rhs1)
  if (!length(rhs1_vars)) {
    stop("At least one RHS variable is required.")
  }
  if (!identical(sort(term_labels1), sort(rhs1_vars))) {
    stop("dbdml currently supports only additive main effects on the RHS.")
  }

  rhs2_vars = character(0)
  if (length(fml)[2] > 1) {
    rhs2 = formula(fml, lhs = 0, rhs = 2)
    term_labels2 = attr(terms(rhs2), "term.labels")
    rhs2_vars = all.vars(rhs2)
    if (!identical(sort(term_labels2), sort(rhs2_vars))) {
      stop("dbdml currently supports only additive main effects after `|`.")
    }
  }

  if (is.null(treat)) {
    treatments = rhs1_vars[1]
  } else {
    if (!is.character(treat)) {
      stop("`treat` must be NULL or a character vector.")
    }
    treatments = unique(treat)
    if (!length(treatments)) {
      stop("`treat` must contain at least one variable name.")
    }
  }

  if (!all(treatments %in% rhs1_vars)) {
    stop("All `treat` variables must appear on the first RHS part of the formula.")
  }

  controls = setdiff(unique(c(rhs1_vars, rhs2_vars)), treatments)

  list(
    fml = fml,
    yvar = yvar,
    treatments = treatments,
    controls = controls,
    rhs1_vars = rhs1_vars,
    rhs2_vars = rhs2_vars
  )
}

#' @keywords internal
parse_dbdml_ridge = function(ridge) {
  if (is.list(ridge)) {
    if (is.null(ridge$y) || is.null(ridge$d)) {
      stop("If `ridge` is a list, it must contain both `y` and `d`.")
    }
    ridge_y = ridge$y
    ridge_d = ridge$d
  } else {
    ridge_y = ridge
    ridge_d = ridge
  }

  for (val in list(ridge_y, ridge_d)) {
    if (!is.numeric(val) || length(val) != 1 || is.na(val) || val < 0) {
      stop("`ridge` must be a non-negative scalar or list(y = ..., d = ...).")
    }
  }

  list(y = as.numeric(ridge_y), d = as.numeric(ridge_d))
}

#' @keywords internal
choose_dbdml_method = function(conn, from_statement, controls, data, vcov, sql_only, data_only) {
  if (!length(controls)) {
    return("plm")
  }

  if (vcov == "bootstrap") {
    return("strata")
  }
  if (vcov == "cluster") {
    return("plm")
  }

  if (sql_only || data_only) {
    return("strata")
  }

  if (dbdml_controls_are_discrete(conn, from_statement, controls, data)) {
    "strata"
  } else {
    "plm"
  }
}

#' @keywords internal
validate_simple_names = function(vars) {
  vars = vars[!is.na(vars)]
  if (!length(vars)) {
    return(invisible(TRUE))
  }
  ok = grepl("^[A-Za-z_][A-Za-z0-9_]*$", vars)
  if (!all(ok)) {
    bad = vars[!ok]
    stop(
      "dbdml currently supports simple variable names only (letters, numbers, underscore). Invalid: ",
      paste(bad, collapse = ", ")
    )
  }
  invisible(TRUE)
}

#' @keywords internal
dbdml_controls_are_discrete = function(conn, from_statement, controls, data = NULL) {
  if (!length(controls)) {
    return(TRUE)
  }

  if (!is.null(data)) {
    data = as.data.frame(data)
    is_discrete = vapply(controls, function(v) {
      xv = data[[v]]
      is.factor(xv) || is.character(xv) || is.logical(xv) || is.integer(xv)
    }, logical(1))
    return(all(is_discrete))
  }

  nd_alias = paste0("nd_", seq_along(controls))
  nd_expr = paste(
    sprintf("COUNT(DISTINCT %s) AS %s", controls, nd_alias),
    collapse = ", "
  )
  sql = glue("SELECT COUNT(*) AS n, {nd_expr} {from_statement}")
  res = tryCatch(dbGetQuery(conn, sql), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) {
    return(FALSE)
  }

  n = as.numeric(res$n[1])
  if (!is.finite(n) || n <= 0) {
    return(FALSE)
  }

  threshold = max(20, ceiling(0.1 * n))
  nd_vals = as.numeric(res[1, nd_alias, drop = TRUE])
  all(nd_vals <= threshold)
}

#' @keywords internal
dbdml_apply_missing_filter = function(from_statement, vars) {
  vars = unique(vars[!is.na(vars)])
  if (!length(vars)) {
    return(from_statement)
  }

  if (grepl("WHERE|LIMIT|ORDER\\s+BY|GROUP\\s+BY|HAVING", from_statement, ignore.case = TRUE)) {
    from_statement = glue("FROM (SELECT * {from_statement}) AS subq")
  }

  where_clause = paste(sprintf("%s IS NOT NULL", vars), collapse = " AND ")
  glue("{from_statement}\nWHERE {where_clause}")
}

#' @keywords internal
dbdml_fit_strata = function(
  yvar,
  treatments,
  strata,
  conn,
  from_statement,
  vcov,
  n_bootstraps,
  seed,
  sql_only,
  data_only,
  drop_missings,
  verbose
) {
  required_vars = c(yvar, treatments, strata)
  if (isTRUE(drop_missings)) {
    from_statement = dbdml_apply_missing_filter(from_statement, required_vars)
  }

  agg_terms = c(
    "COUNT(*) AS n_g",
    glue("SUM({yvar}) AS sum_y"),
    glue("SUM(POWER({yvar}, 2)) AS sum_y_sq")
  )
  for (x in treatments) {
    agg_terms = c(
      agg_terms,
      glue("SUM({x}) AS sum_{x}"),
      glue("SUM({yvar} * {x}) AS sum_{yvar}_{x}")
    )
  }
  for (i in seq_along(treatments)) {
    x1 = treatments[i]
    for (j in i:length(treatments)) {
      x2 = treatments[j]
      agg_terms = c(agg_terms, glue("SUM({x1} * {x2}) AS sum_{x1}_{x2}"))
    }
  }

  select_cols = if (length(strata) > 0) paste(strata, collapse = ", ") else NULL
  select_exprs = c(select_cols, agg_terms)
  group_by_clause = if (length(strata) > 0) paste0("\nGROUP BY ", paste(strata, collapse = ", ")) else ""
  having_clause = if (length(strata) > 0) "\nHAVING COUNT(*) > 1" else ""

  compress_sql = paste0(
    "SELECT\n  ",
    paste(select_exprs, collapse = ",\n  "),
    "\n",
    from_statement,
    group_by_clause,
    having_clause
  )

  if (isTRUE(sql_only)) {
    return(compress_sql)
  }

  if (verbose) {
    message("[dbdml] Executing strata compression SQL")
  }
  compressed = dbGetQuery(conn, compress_sql)
  if (isTRUE(data_only)) {
    return(compressed)
  }

  if (nrow(compressed) == 0) {
    warning("[dbdml] No data after compression. Returning NA estimates.")
    return(dbdml_empty_result(yvar, treatments, strata, compress_sql, method = "strata"))
  }

  if ("n_g" %in% names(compressed)) {
    compressed = compressed[compressed$n_g > 1, , drop = FALSE]
  }
  if (nrow(compressed) == 0) {
    warning("[dbdml] All groups are singletons. Returning NA estimates.")
    return(dbdml_empty_result(yvar, treatments, strata, compress_sql, method = "strata"))
  }

  moments = dbdml_loo_moments(compressed, yvar, treatments)
  beta_hat = dbdml_solve_beta(moments$total_XTX, moments$total_XTY)
  rownames(beta_hat) = treatments

  nobs = sum(moments$n_g)
  df_res = max(nobs - length(treatments), 1)

  vcov_mat = switch(
    vcov,
    "none" = dbdml_vcov_none(length(treatments)),
    "iid" = {
      bread = tryCatch(solve(moments$total_XTX), error = function(e) NULL)
      if (is.null(bread)) {
        dbdml_vcov_none(length(treatments))
      } else {
        rss = dbdml_rss_from_moments(moments, beta_hat)
        sigma2 = rss / df_res
        out = sigma2 * bread
        attr(out, "type") = "iid"
        out
      }
    },
    "hc1" = dbdml_vcov_hc1(moments, beta_hat),
    "bootstrap" = dbdml_vcov_bootstrap(compressed, yvar, treatments, n_bootstraps, seed)
  )
  dimnames(vcov_mat) = list(treatments, treatments)

  coeftable = gen_coeftable(beta_hat, vcov_mat, df_res)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    outcome = yvar,
    treatments = treatments,
    controls = strata,
    strata = strata,
    nobs = nobs,
    n_groups = nrow(compressed),
    df_residual = df_res,
    vcov_type = vcov,
    n_bootstraps = n_bootstraps,
    folds = NA_integer_,
    query_string = compress_sql
  )
}

#' @keywords internal
dbdml_fit_plm = function(
  yvar,
  treatments,
  controls,
  conn,
  from_statement,
  vcov,
  cluster_var,
  folds,
  ridge_y,
  ridge_d,
  drop_missings,
  verbose
) {
  backend = detect_backend(conn)$name
  tmp_stamp = gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%S%OS3"))
  fold_table = dbreg_temp_table_name(paste0("__dbdml_fold_", tmp_stamp), backend)
  resid_table = dbreg_temp_table_name(paste0("__dbdml_resid_", tmp_stamp), backend)

  cleanup_temp = function() {
    dbreg_drop_table(conn, fold_table, backend)
    dbreg_drop_table(conn, resid_table, backend)
  }
  on.exit(cleanup_temp(), add = TRUE)

  needed_vars = c(yvar, treatments, controls, cluster_var)
  if (isTRUE(drop_missings)) {
    from_statement = dbdml_apply_missing_filter(from_statement, needed_vars)
  }

  if (!length(controls)) {
    base_table = paste0("(SELECT * ", from_statement, ") AS __dbdml_base")
    rhs = paste(treatments, collapse = " + ")
    fml2 = as.formula(paste(yvar, "~", rhs, "-1"))
    second = dbreg(
      fml = fml2,
      conn = conn,
      table = base_table,
      vcov = if (vcov == "cluster") "iid" else vcov,
      cluster = cluster_var,
      strategy = "moments",
      ridge = 0,
      drop_missings = FALSE,
      verbose = FALSE
    )
    return(dbdml_from_second_stage(second, treatments, yvar, controls, vcov, folds = NA_integer_))
  }

  fold_sql = paste0(
    "SELECT *, ((ROW_NUMBER() OVER (ORDER BY 1) - 1) % ",
    folds,
    ") + 1 AS __dbdml_fold ",
    from_statement
  )
  dbreg_create_temp_table_as(conn, fold_table, fold_sql, backend)

  nuisance_formula = reformulate(controls)
  fold_selects = vector("list", folds)

  for (k in seq_len(folds)) {
    train_table = paste0("(SELECT * FROM ", fold_table, " WHERE __dbdml_fold <> ", k, ") AS __dbdml_train")

    model_y = dbreg(
      fml = reformulate(controls, response = yvar),
      conn = conn,
      table = train_table,
      vcov = "iid",
      strategy = "moments",
      ridge = ridge_y,
      drop_missings = FALSE,
      verbose = FALSE
    )

    yhat_expr = dbdml_predict_expr(model_y, nuisance_formula, conn, fold_table)

    d_exprs = character(length(treatments))
    names(d_exprs) = treatments

    for (d in treatments) {
      model_d = dbreg(
        fml = reformulate(controls, response = d),
        conn = conn,
        table = train_table,
        vcov = "iid",
        strategy = "moments",
        ridge = ridge_d,
        drop_missings = FALSE,
        verbose = FALSE
      )
      d_exprs[d] = dbdml_predict_expr(model_d, nuisance_formula, conn, fold_table)
    }

    select_cols = c(
      glue("({yvar} - ({yhat_expr})) AS __dbdml_y_tilde")
    )
    for (d in treatments) {
      d_tilde = paste0("__dbdml_", d, "_tilde")
      select_cols = c(select_cols, glue("({d} - ({d_exprs[d]})) AS {d_tilde}"))
    }
    if (!is.null(cluster_var)) {
      select_cols = c(select_cols, glue("{cluster_var} AS __dbdml_cluster"))
    }

    fold_selects[[k]] = paste0(
      "SELECT ",
      paste(select_cols, collapse = ", "),
      " FROM ",
      fold_table,
      " WHERE __dbdml_fold = ",
      k
    )
  }

  residual_sql = paste(fold_selects, collapse = "\nUNION ALL\n")
  dbreg_create_temp_table_as(conn, resid_table, residual_sql, backend)

  rhs_tilde = paste0("__dbdml_", treatments, "_tilde")
  lhs_tilde = "`__dbdml_y_tilde`"
  rhs_tilde_terms = paste(sprintf("`%s`", rhs_tilde), collapse = " + ")
  fml2 = as.formula(paste(lhs_tilde, "~", rhs_tilde_terms, "-1"))

  second = dbreg(
    fml = fml2,
    conn = conn,
    table = resid_table,
    vcov = if (vcov == "cluster") "iid" else vcov,
    cluster = if (!is.null(cluster_var)) "__dbdml_cluster" else NULL,
    strategy = "moments",
    ridge = 0,
    drop_missings = FALSE,
    verbose = FALSE
  )

  out = dbdml_from_second_stage(second, treatments, yvar, controls, vcov, folds = folds)
  out$query_string = residual_sql
  out
}

#' @keywords internal
dbdml_predict_expr = function(model, rhs_formula, conn, table_ref) {
  design = sql_model_matrix(
    formula = rhs_formula,
    conn = conn,
    table = table_ref,
    expand = "all",
    fe_vars = character(0)
  )

  coefs = model$coeftable[, "estimate"]
  coef_names = names(coefs)

  terms = character(0)
  if ("(Intercept)" %in% coef_names) {
    terms = c(terms, sprintf("%.15g", coefs[["(Intercept)"]]))
  }

  for (i in seq_along(design$col_names)) {
    nm = design$col_names[i]
    if (nm %in% coef_names && is.finite(coefs[[nm]])) {
      terms = c(terms, sprintf("(%.15g) * (%s)", coefs[[nm]], design$select_exprs[i]))
    }
  }

  if (!length(terms)) {
    return("0.0")
  }
  paste(terms, collapse = " + ")
}

#' @keywords internal
dbdml_from_second_stage = function(second, treatments, yvar, controls, vcov_type, folds) {
  coef_names = rownames(second$coeftable)
  if (is.null(coef_names)) {
    stop("Second-stage coeftable is missing row names.")
  }

  rhs_terms = vapply(treatments, function(tr) {
    tilde_nm = paste0("__dbdml_", tr, "_tilde")
    if (tilde_nm %in% coef_names) {
      return(tilde_nm)
    }
    if (tr %in% coef_names) {
      return(tr)
    }
    NA_character_
  }, character(1))

  if (anyNA(rhs_terms)) {
    stop("Could not align second-stage coefficients with treatment variable(s).")
  }

  ct = second$coeftable[rhs_terms, , drop = FALSE]
  rownames(ct) = treatments

  vc = second$vcov[rhs_terms, rhs_terms, drop = FALSE]
  dimnames(vc) = list(treatments, treatments)

  if (vcov_type == "none") {
    vc = dbdml_vcov_none(length(treatments))
    dimnames(vc) = list(treatments, treatments)
    ct[, "std.error"] = NA_real_
    ct[, "statistic"] = NA_real_
    ct[, "p.values"] = NA_real_
  }

  list(
    coeftable = ct,
    vcov = vc,
    outcome = yvar,
    treatments = treatments,
    controls = controls,
    strata = character(0),
    nobs = second$nobs_orig,
    n_groups = NA_integer_,
    df_residual = second$df_residual,
    vcov_type = vcov_type,
    n_bootstraps = 0L,
    folds = folds,
    query_string = second$query_string
  )
}

#' @keywords internal
dbdml_loo_moments = function(df, outcome_var, treatment_vars) {
  n_treat = length(treatment_vars)
  n_groups = nrow(df)
  n_g = df[["n_g"]]

  weight = n_g / (n_g - 1)^2

  S_X = do.call(cbind, lapply(treatment_vars, function(x) df[[paste0("sum_", x)]]))
  if (n_treat == 1) {
    S_X = matrix(S_X, ncol = 1)
  }
  S_Y = df[["sum_y"]]
  S_Y_sq = df[["sum_y_sq"]]

  S_XX = array(0, dim = c(n_groups, n_treat, n_treat))
  for (i in seq_along(treatment_vars)) {
    x1 = treatment_vars[i]
    for (j in i:length(treatment_vars)) {
      x2 = treatment_vars[j]
      col = paste0("sum_", x1, "_", x2)
      S_XX[, i, j] = df[[col]]
      S_XX[, j, i] = df[[col]]
    }
  }

  S_XY = array(0, dim = c(n_groups, n_treat, 1))
  for (i in seq_along(treatment_vars)) {
    x = treatment_vars[i]
    col = paste0("sum_", outcome_var, "_", x)
    S_XY[, i, 1] = df[[col]]
  }

  Q_WW = array(0, dim = c(n_groups, n_treat, n_treat))
  Q_XY = array(0, dim = c(n_groups, n_treat, 1))
  Q_YY = array(0, dim = c(n_groups, 1, 1))
  for (g in seq_len(n_groups)) {
    sx = matrix(S_X[g, ], ncol = 1)
    sy = S_Y[g]
    sxx = S_XX[g, , ]
    sxy = matrix(S_XY[g, , 1], ncol = 1)
    w = weight[g]
    ng = n_g[g]

    Q_WW[g, , ] = w * (ng * sxx - tcrossprod(sx))
    Q_XY[g, , 1] = as.numeric(w * (ng * sxy - sx * sy))
    Q_YY[g, 1, 1] = w * (ng * S_Y_sq[g] - sy^2)
  }

  total_XTX = matrix(0, n_treat, n_treat)
  total_XTY = matrix(0, n_treat, 1)
  for (g in seq_len(n_groups)) {
    total_XTX = total_XTX + Q_WW[g, , ]
    total_XTY = total_XTY + Q_XY[g, , ]
  }

  list(
    n_g = n_g,
    total_XTX = total_XTX,
    total_XTY = total_XTY,
    Q_WW = Q_WW,
    Q_XY = Q_XY,
    Q_YY = Q_YY
  )
}

#' @keywords internal
dbdml_solve_beta = function(XTX, XTY) {
  beta_hat = tryCatch(
    solve(XTX, XTY),
    error = function(e) matrix(NA_real_, nrow = nrow(XTX), ncol = 1)
  )
  rownames(beta_hat) = rownames(XTX)
  beta_hat
}

#' @keywords internal
dbdml_vcov_none = function(n_treat) {
  vcov_mat = matrix(NA_real_, nrow = n_treat, ncol = n_treat)
  attr(vcov_mat, "type") = "none"
  vcov_mat
}

#' @keywords internal
dbdml_rss_from_moments = function(moments, beta_hat) {
  beta_vec = as.numeric(beta_hat)
  Q_WW = moments$Q_WW
  Q_XY = moments$Q_XY
  Q_YY = moments$Q_YY
  n_groups = dim(Q_WW)[1]

  rss = 0
  for (g in seq_len(n_groups)) {
    term2 = 2 * (t(Q_XY[g, , ]) %*% beta_vec)
    term3 = t(beta_vec) %*% Q_WW[g, , ] %*% beta_vec
    rss = rss + as.numeric(Q_YY[g, 1, 1] - term2 + term3)
  }
  as.numeric(rss)
}

#' @keywords internal
dbdml_vcov_hc1 = function(moments, beta_hat) {
  Q_WW = moments$Q_WW
  Q_XY = moments$Q_XY
  Q_YY = moments$Q_YY
  n_g = moments$n_g
  total_XTX = moments$total_XTX

  beta_vec = as.numeric(beta_hat)
  n_groups = length(n_g)
  n_treat = length(beta_vec)

  bread = tryCatch(solve(total_XTX), error = function(e) NULL)
  if (is.null(bread)) {
    return(dbdml_vcov_none(n_treat))
  }

  SSR_g = numeric(n_groups)
  for (g in seq_len(n_groups)) {
    term2 = 2 * (t(Q_XY[g, , ]) %*% beta_vec)
    term3 = t(beta_vec) %*% Q_WW[g, , ] %*% beta_vec
    SSR_g[g] = as.numeric(Q_YY[g, 1, 1] - term2 + term3)
  }

  sigma_sq_g = SSR_g / n_g

  meat = matrix(0, n_treat, n_treat)
  for (g in seq_len(n_groups)) {
    meat = meat + sigma_sq_g[g] * Q_WW[g, , ]
  }

  vcov_mat = bread %*% meat %*% bread
  N = sum(n_g)
  K = n_treat
  vcov_mat = vcov_mat * (N / (N - K))
  attr(vcov_mat, "type") = "hc1"
  vcov_mat
}

#' @keywords internal
dbdml_vcov_bootstrap = function(df, outcome_var, treatment_vars, n_bootstraps, seed) {
  n_groups = nrow(df)
  n_treat = length(treatment_vars)

  old_seed = if (exists(".Random.seed", envir = .GlobalEnv)) .Random.seed else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(old_seed)) {
      .Random.seed <<- old_seed
    } else {
      rm(.Random.seed, envir = .GlobalEnv)
    }
  }, add = TRUE)

  boot_coefs = matrix(NA_real_, nrow = n_bootstraps, ncol = n_treat)
  for (b in seq_len(n_bootstraps)) {
    idx = sample.int(n_groups, size = n_groups, replace = TRUE)
    df_boot = df[idx, , drop = FALSE]
    moments = dbdml_loo_moments(df_boot, outcome_var, treatment_vars)
    beta_hat = dbdml_solve_beta(moments$total_XTX, moments$total_XTY)
    boot_coefs[b, ] = as.numeric(beta_hat)
  }

  vcov_mat = stats::cov(boot_coefs)
  if (is.null(dim(vcov_mat))) {
    vcov_mat = matrix(vcov_mat, nrow = 1, ncol = 1)
  }
  attr(vcov_mat, "type") = "bootstrap"
  vcov_mat
}

#' @keywords internal
dbdml_empty_result = function(yvar, treatments, strata, query, method = "strata") {
  n_treat = length(treatments)
  vcov_mat = dbdml_vcov_none(n_treat)
  dimnames(vcov_mat) = list(treatments, treatments)
  beta_hat = matrix(NA_real_, nrow = n_treat, ncol = 1)
  rownames(beta_hat) = treatments

  coeftable = gen_coeftable(beta_hat, vcov_mat, 1)

  list(
    coeftable = coeftable,
    vcov = vcov_mat,
    outcome = yvar,
    treatments = treatments,
    controls = strata,
    strata = strata,
    nobs = 0L,
    n_groups = 0L,
    df_residual = NA_real_,
    vcov_type = "none",
    n_bootstraps = 0L,
    folds = NA_integer_,
    query_string = query,
    method = method
  )
}

#' Print method for dbdml objects
#' @param x A `dbdml` object.
#' @param ... Unused.
#' @export
print.dbdml = function(x, ...) {
  if (identical(x$method, "strata")) {
    cat("Strata LOO partialling-out estimator\n")
    cat("Outcome:", x$outcome, "\n")
    cat("Treatments:", paste(x$treatments, collapse = ", "), "\n")
    if (length(x$controls) > 0) {
      cat("Strata controls:", paste(x$controls, collapse = ", "), "\n")
    } else {
      cat("Strata controls: (none)\n")
    }
    cat("Observations:", prettyNum(x$nobs, big.mark = ","), "\n")
    cat("Groups:", prettyNum(x$n_groups, big.mark = ","), "\n")
  } else {
    cat("Cross-fitted partially linear model estimator\n")
    cat("Outcome:", x$outcome, "\n")
    cat("Treatments:", paste(x$treatments, collapse = ", "), "\n")
    if (length(x$controls) > 0) {
      cat("Controls:", paste(x$controls, collapse = ", "), "\n")
    } else {
      cat("Controls: (none)\n")
    }
    cat("Observations:", prettyNum(x$nobs, big.mark = ","), "\n")
    if (!is.na(x$folds)) {
      cat("Folds:", x$folds, "\n")
    }
  }

  se_type = switch(
    x$vcov_type,
    "iid" = "IID",
    "hc1" = "HC1",
    "cluster" = "Clustered",
    "bootstrap" = "Bootstrap",
    "none" = "None",
    toupper(x$vcov_type)
  )
  cat("Standard Errors:", se_type, "\n")
  print_coeftable(x$coeftable, has_fes = FALSE, gof_vals = NULL)
  invisible(x)
}

#' Extract coefficients from dbdml objects
#' @param object A `dbdml` object.
#' @param ... Unused.
#' @export
coef.dbdml = function(object, ...) {
  out = object[["coeftable"]][, "estimate"]
  names(out) = rownames(object[["coeftable"]])
  out
}

#' Variance-covariance matrix for dbdml objects
#' @param object A `dbdml` object.
#' @param ... Unused.
#' @export
vcov.dbdml = function(object, ...) {
  object[["vcov"]]
}

#' Confidence intervals for dbdml objects
#' @param object A `dbdml` object.
#' @param parm Subset of parameters to return.
#' @param level Confidence level.
#' @param ... Unused.
#' @export
confint.dbdml = function(object, parm, level = 0.95, ...) {
  ct = object[["coeftable"]]
  cf = ct[, "estimate"]
  ses = ct[, "std.error"]
  df = object$df_residual

  a = (1 - level) / 2
  t_crit = qt(1 - a, df)
  ci = cbind(cf - t_crit * ses, cf + t_crit * ses)
  rownames(ci) = rownames(ct)
  colnames(ci) = sprintf("%.1f %%", 100 * c(a, 1 - a))

  if (!missing(parm)) {
    ci = ci[parm, , drop = FALSE]
  }
  ci
}
