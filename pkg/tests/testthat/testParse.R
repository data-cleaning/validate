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
test_that("simple freeform parsing", {
#  expect_equal( length( validator(.file="yamltests/freeform.yaml") ) , 2)
})

test_that("Parsing included files",{
  v <- validator(.file="yamltests//top.yaml")
})






