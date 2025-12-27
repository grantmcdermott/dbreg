
if ( requireNamespace("tinytest", quietly=TRUE) ){
  dvo = getOption("dbreg.verbose")
  options(dbreg.verbose = FALSE)
  tinytest::test_package("dbreg")
  options(dvo)
}

