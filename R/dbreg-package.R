#' @keywords internal
#' @importFrom utils globalVariables
"_PACKAGE"

# Silence R CMD check NOTEs for variables it cannot see
globalVariables(c(
  # NSE in dbbinsreg / plot
  "bin",
  "x_mid",
  "u",
  "u2",
  "V_beta",
  "cb_lwr",
  "cb_upr",
  # Fields pulled from the `inputs` environment into local scope via env2env()
  "any_continuous",
  "caller",
  "cluster_var",
  "compress_nmax",
  "compress_ratio",
  "compression_ratio_est",
  "conn",
  "data",
  "data_only",
  "drop_missings",
  "endog_names",
  "endog_vars",
  "exog_names",
  "fe",
  "fml",
  "from_statement",
  "has_interactions",
  "instr_names",
  "instr_vars",
  "intercept",
  "path",
  "sql_only",
  "ssc",
  "strategy",
  "structural_fml",
  "union_names",
  "union_sql",
  "vcov_type_req",
  "verbose",
  "weights",
  "x_all_names",
  "xvars",
  "yvar",
  "z_all_names"
))
