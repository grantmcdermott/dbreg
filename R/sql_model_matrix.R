#' Construct SQL expressions for a design matrix
#'
#' Expands formula terms into SQL SELECT expressions, handling factor one-hot
#' encoding and interaction terms. Only the right-hand side of the formula is
#' processed; the response variable (LHS) is ignored.
#'
#' Terms can also use `fixest`-style `i()` syntax: `i(f)` expands `f` like a
#' factor (numeric `f` included), and `i(f, x)` interacts each level of `f`
#' with a numeric or logical `x`. `ref` drops the given level(s) and `keep`
#' keeps only the given levels. Columns are named as in `fixest`, e.g.
#' `period::5:treat`. One difference: in a model without an intercept, a plain
#' `i(f)` drops its first level if another factor comes first, whereas
#' `fixest` drops its last level (via collinearity) instead.
#'
#' @param formula A formula (or Formula) object. Only RHS terms are expanded;
#'   the LHS (response variable) is ignored.
#' @param conn Database connection
#' @param table Table name or FROM clause
#' @param expand Character: `"all"` expands factors and interactions, 
#'   `"interactions"` only expands interaction terms (factors in main effects 
#'   kept as-is for grouping)
#' @param sep Character separator for interaction term names. Default is
#'   `"_x_"`. Use `":"` for standard R naming convention.
#' @param fe_vars Character vector of fixed effect variable names. These are
#'   treated as "in the model" for determining whether to drop reference levels
#'   in interactions.
#' @param intercept Logical. Whether the model has an intercept. As in
#'   [stats::model.matrix()], without an intercept the first factor main effect
#'   keeps all of its levels (no reference level is dropped).
#' @return List with:
#'   - `select_exprs`: character vector of SQL expressions
#'   - `col_names`: corresponding column names
#'   - `aliases`: SQL-safe versions of `col_names` (identical unless, e.g.,
#'     factor levels contain spaces or other special characters)
#'   - `level_aliases`: data frame mapping each factor dummy `alias` to its
#'     `var` and `level`
#'   - `factor_levels`: list of factor levels by variable (for reference)
#'   - `i_columns`: data frame mapping each `i()` column `alias` to its factor
#'     `var`, `level` and interacted variable `by` (`NA` if none)
#' @importFrom stats terms
#' @export
#' @examples
#' library(DBI)
#' library(duckdb)
#' con = dbConnect(duckdb())
#' duckdb_register(con, "test", data.frame(x1 = 1:3, x2 = c("a", "b", "c")))
#' sql_model_matrix(~ x1 + x2, con, "test")
#' sql_model_matrix(~ x1:x2, con, "test")
#' sql_model_matrix(~ x1:x2, con, "test", sep = ":")
#' sql_model_matrix(~ i(x2, x1, ref = "a"), con, "test")
#' dbDisconnect(con)
sql_model_matrix = function(
  formula,
  conn,
  table,
  expand = c("all", "interactions"),
  sep = "_x_",
  fe_vars = character(),
  intercept = TRUE
) {
  expand = match.arg(expand)
  
  # Extract terms from formula (handles Formula objects too)
  if (inherits(formula, "Formula")) {
    formula = formula(formula, lhs = 0, rhs = 1)
  }
  tt = terms(formula)
  term_labels = attr(tt, "term.labels")
  
  if (!length(term_labels)) {
    return(list(
      select_exprs = character(), col_names = character(), aliases = character(),
      factor_levels = list(), level_aliases = data.frame(alias = character(), var = character(), level = character()),
      i_columns = empty_i_columns()
    ))
  }
  env = environment(formula)
  i_specs = lapply(term_labels, parse_i_term, env = if (is.null(env)) baseenv() else env)

  # Get column types and factor levels from database (an i() factor is
  # expanded by level whatever its type)
  term_vars = Map(function(t, spec) if (is.null(spec)) strsplit(t, ":")[[1]] else c(spec[["f"]], spec[["var"]]), term_labels, i_specs)
  i_factors = unlist(lapply(i_specs, `[[`, "f"))
  col_info = get_column_info(conn, table, unique(unlist(term_vars)), level_vars = i_factors)

  # SQL-safe aliases for factor dummies, one per (variable, level), computed
  # once so that every term uses the same alias. They avoid the model's
  # variable names and each other, and follow the backend's case rules.
  fold_case = folds_identifier_case(conn)
  taken = names(col_info[["types"]])
  level_aliases = list()
  for (v in names(col_info[["levels"]])) {
    lvls = c(as.character(col_info[["levels"]][[v]]), "_constant")
    a = safe_identifiers(paste0(v, lvls), taken = taken, fold_case = fold_case)
    taken = c(taken, a)
    level_aliases[[v]] = data.frame(alias = a, var = v, level = lvls)
  }
  level_aliases = do.call(rbind, c(list(data.frame(alias = character(), var = character(), level = character())), level_aliases))
  col_info[["level_aliases"]] = level_aliases

  result = list(select_exprs = character(), col_names = character(), aliases = character())
  
  # Without an intercept, the first factor main effect (incl. a plain i(f))
  # keeps all its levels
  full_factor = if (!intercept) {
    is_factor_main = vapply(seq_along(term_labels), function(k) {
      t = term_labels[k]
      spec = i_specs[[k]]
      if (!is.null(spec)) return(is.null(spec[["var"]]) && is.null(spec[["ref"]]))
      !grepl(":", t, fixed = TRUE) && identical(col_info[["types"]][[t]], "factor")
    }, logical(1))
    term_labels[is_factor_main][1]
  }
  i_columns = empty_i_columns()
  for (k in seq_along(term_labels)) {
    term = term_labels[k]
    if (!is.null(i_specs[[k]])) {
      expanded = expand_i_term(i_specs[[k]], col_info, k, keep_all_levels = identical(term, full_factor))
      i_columns = rbind(i_columns, expanded[["columns"]])
    } else {
      expanded = expand_term(
        term, col_info, expand, sep,
        all_terms = unique(c(term_labels, fe_vars)),
        fe_vars = fe_vars,
        keep_all_levels = identical(term, full_factor)
      )
    }
    result[["select_exprs"]] = c(result[["select_exprs"]], expanded[["sql"]])
    result[["col_names"]] = c(result[["col_names"]], expanded[["name"]])
    result[["aliases"]] = c(result[["aliases"]], expanded[["alias"]])
  }
  
  result[["factor_levels"]] = col_info[["levels"]]
  result[["level_aliases"]] = level_aliases
  result[["i_columns"]] = i_columns
  result
}

#' Get column types and factor levels from database
#' @keywords internal
get_column_info = function(conn, table, vars, level_vars = character()) {
  # Build FROM clause
  from_sql = if (grepl("^FROM", table, ignore.case = TRUE)) table else paste("FROM", table)
  
  # Query to get types and sample values
  # Fetch zero rows to get the schema, then separate queries for factor levels
  # (sql_limit() handles dialects without LIMIT, e.g. SQL Server's TOP)
  backend = detect_backend(conn)[["name"]]
  schema_query = sql_limit(glue("SELECT {paste(vars, collapse = ', ')} {from_sql}"), 0, backend)
  schema = dbGetQuery(conn, schema_query)
  
  types = list()
  levels = list()
  classes = list()
  
  for (v in vars) {
    col_class = class(schema[[v]])[1]
    classes[[v]] = col_class
    # Treat character, factor, logical as categorical
    is_cat = col_class %in% c("character", "factor", "logical")
    types[[v]] = if (is_cat) "factor" else "numeric"
    if (is_cat || v %in% level_vars) {
      # All distinct levels (a sample could miss rare levels)
      lvl_query = glue("SELECT DISTINCT {v} FROM (SELECT {v} {from_sql}) lvl WHERE {v} IS NOT NULL ORDER BY {v}")
      levels[[v]] = dbGetQuery(conn, lvl_query)[[1]]
    }
  }
  
  list(types = types, levels = levels, classes = classes)
}

#' Expand a single term into SQL expressions
#'
#' Expansions are lists of parallel character vectors: `sql` (expressions),
#' `name` (column names) and `alias` (SQL-safe column names).
#' @keywords internal
expand_term = function(term, col_info, expand, sep = "_x_", all_terms = character(), fe_vars = character(), keep_all_levels = FALSE) {
  vars = strsplit(term, ":")[[1]]
  is_interaction = length(vars) > 1
  
  if (!is_interaction && expand == "interactions") {
    # Main effect, only expanding interactions: return as-is
    return(list(sql = term, name = term, alias = term))
  }
  
  # Expand each variable
  expansions = lapply(vars, function(v) {
    drop_ref = !keep_all_levels
    if (is_interaction && identical(col_info[["types"]][[v]], "factor")) {
      other_term = paste(sort(setdiff(vars, v)), collapse = ":")
      drop_ref = v %in% fe_vars || other_term %in% all_terms
    }
    expand_variable(v, col_info, expand, is_interaction, drop_ref)
  })
  
  # Cross product for interactions
  Reduce(function(a, b) cross_product(a, b, sep), expansions, right = TRUE)
}

#' Expand a single variable into SQL expression(s)
#' @keywords internal
expand_variable = function(var, col_info, expand, in_interaction, drop_ref = TRUE) {
  is_factor = identical(col_info[["types"]][[var]], "factor")
  
  # Only expand factors if: expand="all", OR variable is part of interaction
  if (!(is_factor && (expand == "all" || in_interaction))) {
    # Numeric or unexpanded factor: pass through
    return(list(sql = var, name = var, alias = var))
  }
  lvls = col_info[["levels"]][[var]]
  la = col_info[["level_aliases"]]
  la = la[la[["var"]] == var, , drop = FALSE]
  if (length(lvls) < 2) {
    return(list(sql = "1", name = paste0(var, "_constant"), alias = la[["alias"]][la[["level"]] == "_constant"]))
  }
  # Drop first level (reference) only if appropriate
  if (drop_ref) {
    lvls = lvls[-1]
  }
  list(
    # Escape single quotes in level names
    sql = paste0("CASE WHEN ", var, " = '", gsub("'", "''", lvls, fixed = TRUE), "' THEN 1.0 ELSE 0.0 END"),
    name = paste0(var, lvls),
    alias = la[["alias"]][match(lvls, la[["level"]])]
  )
}

#' Cartesian product of two term expansions
#'
#' Columns of `a` vary slowest (as in a nested loop over `a`, then `b`).
#' @keywords internal
cross_product = function(a, b, sep = "_x_") {
  i = rep(seq_along(a[["sql"]]), each = length(b[["sql"]]))
  j = rep(seq_along(b[["sql"]]), times = length(a[["sql"]]))
  list(
    sql = paste0("(", a[["sql"]][i], ") * (", b[["sql"]][j], ")"),
    name = paste0(a[["name"]][i], sep, b[["name"]][j]),
    alias = paste0(a[["alias"]][i], "_x_", b[["alias"]][j])
  )
}

empty_i_columns = function() {
  data.frame(alias = character(), var = character(), level = character(), by = character())
}

# Argument template for i() calls (as in fixest::i())
i_template = function(factor_var, var, ref, keep, bin, ref2, keep2, bin2) NULL

#' Parse a `fixest`-style `i()` term
#'
#' @param term A term label.
#' @param env Environment in which to evaluate `ref` and `keep` (e.g. the
#'   formula's), or `NULL` to only validate the term.
#' @return `NULL` if `term` is not an `i()` call, else a list with the factor
#'   `f`, interacted variable `var` and the `ref` and `keep` levels (each
#'   `NULL` if absent or not evaluated).
#' @keywords internal
parse_i_term = function(term, env = NULL) {
  e = tryCatch(str2lang(term), error = function(e) NULL)
  if (!is.call(e) || !identical(e[[1]], as.name("i"))) {
    return(NULL)
  }
  e = tryCatch(match.call(i_template, e), error = function(err) {
    stop("Invalid i() term: ", term, " (", conditionMessage(err), ")", call. = FALSE)
  })
  unsupported = intersect(names(e), c("bin", "ref2", "keep2", "bin2"))
  if (length(unsupported)) {
    stop("i() arguments not supported yet: ", paste(unsupported, collapse = ", "), call. = FALSE)
  }
  args = list(e[["factor_var"]], e[["var"]])
  if (any(vapply(args, function(x) is.name(x) && startsWith(as.character(x), "i."), logical(1)))) {
    stop("The i. prefix in i() terms is not supported yet: ", term, call. = FALSE)
  }
  if (!is.name(args[[1]]) || !(is.null(args[[2]]) || is.name(args[[2]]))) {
    stop("i() terms must take plain variable names, e.g. i(period, treat, ref = 5): ", term, call. = FALSE)
  }
  value = function(arg) {
    x = e[[arg]]
    if (is.null(x) || is.null(env)) return(NULL)
    x = tryCatch(eval(x, env), error = function(err) {
      stop("Could not evaluate `", arg, "` in ", term, ": ", conditionMessage(err), call. = FALSE)
    })
    x = as.character(x)
    if (any(startsWith(x, "@"))) {
      stop("Regular expressions (\"@...\") in i() `", arg, "` are not supported yet: ", term, call. = FALSE)
    }
    x
  }
  list(
    f = as.character(e[["factor_var"]]),
    var = if (!is.null(e[["var"]])) as.character(e[["var"]]),
    ref = value("ref"),
    keep = value("keep")
  )
}

#' Expand an `i()` term into SQL expressions
#'
#' Without `var` or `ref`, `i(f)` drops the first level, like a factor main
#' effect (unless `keep_all_levels`). Columns get opaque aliases, since level
#' text is arbitrary.
#' @param spec Result of [parse_i_term()].
#' @param k Term index (to make aliases unique).
#' @keywords internal
expand_i_term = function(spec, col_info, k, keep_all_levels = FALSE) {
  f = spec[["f"]]
  var = spec[["var"]]
  lvls = col_info[["levels"]][[f]]
  if (inherits(lvls, "POSIXt")) {
    stop("Date-time variables can't be i() factors yet: ", f, ". Convert to a date or character first.", call. = FALSE)
  }
  lvl_chr = as.character(lvls)
  # As in fixest: ref values must match some level; with neither ref nor keep,
  # a plain i(f) drops its first level like a factor main effect (any other
  # collinearity is left to the fit)
  keep = rep(TRUE, length(lvls))
  if (!is.null(spec[["ref"]])) {
    if (!any(lvl_chr %in% spec[["ref"]])) {
      stop("The `ref` value(s) in i(", f, ", ...) match no level of ", f, ".", call. = FALSE)
    }
    keep = !lvl_chr %in% spec[["ref"]]
  } else if (is.null(var) && is.null(spec[["keep"]]) && !keep_all_levels) {
    keep[1] = FALSE
  }
  if (!is.null(spec[["keep"]])) {
    keep = keep & lvl_chr %in% spec[["keep"]]
  }
  lvls = lvls[keep]
  lvl_chr = lvl_chr[keep]
  if (!length(lvls)) {
    stop("i(", f, ", ...) has no levels left after applying ref/keep.", call. = FALSE)
  }
  # Numeric levels are compared unquoted (doubles at full precision, since
  # as.character() rounds to 15 digits); others as (escaped) strings
  literal = if (is.double(lvls) && !inherits(lvls, "Date")) {
    sprintf("%.17g", lvls)
  } else if (is.numeric(lvls)) {
    lvl_chr
  } else {
    paste0("'", gsub("'", "''", lvl_chr, fixed = TRUE), "'")
  }
  cond = paste0(f, " = ", literal)
  if (is.null(var)) {
    sql = paste0("CASE WHEN ", cond, " THEN 1.0 ELSE 0.0 END")
  } else if (identical(col_info[["classes"]][[var]], "logical")) {
    sql = paste0("CASE WHEN ", cond, " AND ", var, " THEN 1.0 ELSE 0.0 END")
  } else if (identical(col_info[["types"]][[var]], "numeric")) {
    sql = paste0("CASE WHEN ", cond, " THEN ", var, " ELSE 0 END")
  } else {
    stop("The second argument of i() must be numeric or logical, but '", var, "' is not.", call. = FALSE)
  }
  alias = sprintf("dbreg_i%d_%d", k, seq_along(lvls))
  name = paste0(f, "::", lvl_chr, if (!is.null(var)) paste0(":", var))
  list(
    sql = sql,
    name = name,
    alias = alias,
    columns = data.frame(alias = alias, var = f, level = lvl_chr, by = if (is.null(var)) NA_character_ else var)
  )
}
