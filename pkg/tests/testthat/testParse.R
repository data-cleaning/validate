context("Parsing files")

test_that("file paths are interpreted correctly",{
  expect_true(is_full_path("C:/hello"))
  expect_true(is_full_path("//server/hello"))
  expect_true(is_full_path("~/hello"))
  expect_true(is_full_path("http://hello"))
  expect_false(is_full_path("./hello"))
  expect_false(is_full_path("reldir/hello"))
  # windoze flavor
  expect_true(is_full_path("C:\\hello"))
  expect_true(is_full_path("\\\\server\\hello"))
  expect_true(is_full_path("~\\hello"))
  expect_false(is_full_path("reldir\\hello"))
})

# TODO: add file parsing tests
#setwd("pkg/tests/testthat/")
test_that("Parsing freeform", {
  expect_equal( length( validator(.file="yamltests/freeform.yaml") ) , 2,info="freeform")
})

test_that("Parsing yrf format", {
  now <- Sys.time()
  v <- validator(.file="yamltests/yamlrules.yaml")
  expect_equal(length(v),2)
  expect_equal(names(v),c("sumrule","conditional"))
  expect_equal(origin(v),c("yamltests/yamlrules.yaml","yamltests/yamlrules.yaml"))
  expect_equal(short(v),c("sum of x and y","if x positive then y also"))
  expect_equal(long(v),c("a looong description here","a looong description here\n"))
  expect_true(all(created(v)-now < 10))
  expect_warning(validator(x?y))
  expect_warning(validator(.file="yamltests/invalid.yaml"))
  expect_warning(validator(.file="yamltests/invalidR.yaml"))
})


test_that("Parsing options",{
  v <- validator(.file="yamltests/yamloptions.yaml") 
  expect_equal(validate_options(v,"raise"),"all")
  expect_equal(length(v),1)
})

test_that("Parsing included files",{
  v <- validator(.file="yamltests/top.yaml")
  expect_equal(length(v),6)
  expect_equal(origin(v)
    , c(  "yamltests/child1.yaml"
        , "yamltests/child1.yaml"
        , "yamltests/child3.yaml"
        , "yamltests/child2.yaml"
        , "yamltests/child2.yaml"
        , "yamltests/top.yaml")
    , info = "file inclusion order"
    )
})






