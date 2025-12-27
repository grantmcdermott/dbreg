#' Operations on package load
#' @param libname library name
#' @param pkgname package name
#' @keywords internal
#' @noRd
.onLoad = function(libname, pkgname) {

  op = options()

  dbreg_op = list(
    dbreg.verbose = FALSE
  )

  to_set = dbreg_op[!(names(dbreg_op) %in% names(op))]
  if (length(to_set)) options(to_set)

  invisible()
}
