
if ( requireNamespace("tinytest", quietly=TRUE) ){
  # redirect plots
  pdf(file=tempfile())
  tinytest::test_package("validate")
}

