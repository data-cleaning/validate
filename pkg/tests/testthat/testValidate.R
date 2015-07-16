
context("Validation outcomes")

test_that('keys are copied to rownames',{
  w <- women
  w$code <- letters[1:15]
  expect_equal(
    rownames(values(confront(w,validator(height > weight),key='code'))) ,w$code
  )
  expect_equal(
    rownames(values(confront(w,indicator(sqrt(height)),key='code'))), w$code
  )
})




