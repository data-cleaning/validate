
context("Property retrieval")

test_that("properties can be retrieved",{
  expect_true( variables(validator(x > 0)) == 'x')
  expect_true( all( sort(variables(validator(x > 0, y > 0))) == c('x','y') ) )
  expect_true( variables(validator(x>0, x<1 )) == 'x')
  expect_true( all( sort(variables(validator(x +y > 0, y < 1))) == c('x','y') ) )
})



