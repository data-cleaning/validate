
context("Validation outcomes")


test_that('keys are copied to rownames',{
  w <- women
  w$code <- letters[1:15]
  expect_equal(
    rownames(values(confront(validator(height > weight),w,key='code'))) ,w$code
  )
  expect_equal(
    rownames(values(confront(indicator(sqrt(height)),w,key='code'))), w$code
  )
})




