#' Build SQL expressions equivalent to model.matrix()
#'
#' Expands formula terms into SQL SELECT expressions, handling factor one-hot
#' encoding and interaction terms.
#'
#' @param formula A formula (or Formula) object
#' @param conn Database connection
#' @param table Table name or FROM clause
#' @param expand Character: "all" expands factors and interactions, 
#'   "interactions" only expands interaction terms (factors in main effects 
#'   kept as-is for grouping)
#' @return List with:
#'   - `select_exprs`: character vector of SQL expressions
#'   - `col_names`: corresponding column names
#'   - `factor_levels`: list of factor levels by variable (for reference)
#' @export
#' @examples
#' library(DBI)
#' library(duckdb)
#' con = dbConnect(duckdb())
#' duckdb_register(con, "test", data.frame(x1 = 1:3, x2 = c("a", "b", "c")))
#' sql_model_matrix(~ x1 + x2, con, "test")
#' sql_model_matrix(~ x1:x2, con, "test")
#' dbDisconnect(con)
sql_model_matrix = function(
  formula,
  conn,
  table,
  expand = c("all", "interactions")
) {
  expand = match.arg(expand)
  
  # Extract terms from formula (handles Formula objects too)
  if (inherits(formula, "Formula")) {
    formula = formula(formula, lhs = 0, rhs = 1)
  }
  tt = terms(formula)
  term_labels = attr(tt, "term.labels")
  
  if (!length(term_labels)) {
    return(list(select_exprs = character(), col_names = character(), factor_levels = list()))
  }
  
  # Get column types and factor levels from database
  col_info = get_column_info(conn, table, term_labels)
  
  result = list(select_exprs = character(), col_names = character())
  
  for (term in term_labels) {
    expanded = expand_term(term, col_info, expand)
    result$select_exprs = c(result$select_exprs, vapply(expanded, `[[`, character(1), "sql"))
    result$col_names = c(result$col_names, vapply(expanded, `[[`, character(1), "name"))
  }
  
  result$factor_levels = col_info$levels
  result
}

#' Get column types and factor levels from database
#' @keywords internal
get_column_info = function(conn, table, term_labels) {
  # Extract unique variable names from all terms
  vars = unique(unlist(lapply(term_labels, function(t) strsplit(t, ":")[[1]])))
  
  # Build FROM clause
  from_sql = if (grepl("^FROM", table, ignore.case = TRUE)) table else paste("FROM", table)
  
  # Query to get types and sample values
  # Use LIMIT 0 to get schema, then separate queries for factor levels
  schema_query = glue("SELECT {paste(vars, collapse = ', ')} {from_sql} LIMIT 0")
  schema = dbGetQuery(conn, schema_query)
  
  types = list()
  levels = list()
  
  for (v in vars) {
    col_class = class(schema[[v]])[1]
    # Treat character, factor, logical as categorical
    if (col_class %in% c("character", "factor", "logical")) {
      types[[v]] = "factor"
      # Get distinct levels
      lvl_query = glue("SELECT DISTINCT {v} FROM ({from_sql} LIMIT 100000) WHERE {v} IS NOT NULL ORDER BY {v}")
      levels[[v]] = dbGetQuery(conn, lvl_query)[[1]]
    } else {
      types[[v]] = "numeric"
    }
  }
  
  list(types = types, levels = levels)
}

#' Expand a single term into SQL expressions
#' @keywords internal
expand_term = function(term, col_info, expand) {
  vars = strsplit(term, ":")[[1]]
  is_interaction = length(vars) > 1
  
  if (!is_interaction && expand == "interactions") {
    # Main effect, only expanding interactions: return as-is
    return(list(list(sql = term, name = term)))
  }
  
  # Expand each variable
  expansions = lapply(vars, function(v) {
    expand_variable(v, col_info, expand, is_interaction)
  })
  
  # Cross product for interactions
  cross_product(expansions)
}

#' Expand a single variable into SQL expression(s)
#' @keywords internal
expand_variable = function(var, col_info, expand, in_interaction) {
  is_factor = identical(col_info$types[[var]], "factor")
  
  # Only expand factors if: expand="all", OR variable is part of interaction
  if (is_factor && (expand == "all" || in_interaction)) {
    lvls = col_info$levels[[var]]
    if (length(lvls) < 2) {
      return(list(list(sql = "1", name = paste0(var, "_constant"))))
    }
    # Drop first level (reference)
    lvls = lvls[-1]
    lapply(lvls, function(lvl) {
      # Escape single quotes in level names
      lvl_escaped = gsub("'", "''", lvl)
      list(
        sql = glue("CASE WHEN {var} = '{lvl_escaped}' THEN 1.0 ELSE 0.0 END"),
        name = paste0(var, lvl)
      )
    })
  } else {
    # Numeric or unexpanded factor: pass through
    list(list(sql = var, name = var))
  }
}

#' Cartesian product of term expansions
#' @keywords internal
cross_product = function(expansions) {
  if (length(expansions) == 1) return(expansions[[1]])
  
  # Recursive: cross first with rest
  rest = cross_product(expansions[-1])
  
  result = list()
  for (e1 in expansions[[1]]) {
    for (e2 in rest) {
      result = c(result, list(list(
        sql = glue("({e1$sql}) * ({e2$sql})"),
        name = paste0(e1$name, "_", e2$name)
      )))
    }
  }
  result
}

