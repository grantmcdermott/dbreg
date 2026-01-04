#' @keywords internal
"_PACKAGE"

# Silence R CMD check NOTEs for NSE
utils::globalVariables(c(
 "bin", "x_mid", "u", "u2",      # dbbinsreg
 "V_beta", "cb_lwr", "cb_upr"    # dbbinsreg / plot
))
