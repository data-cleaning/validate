
context('Options')

test_that("Options can be set locally",{
  v <- validator()
  validate_options(v, raise='all')
  expect_false(validate_options()$raise == validate_options(v)$raise)
})

test_that("Options can be executed locally without side effects",{
  v <- validator(x > 0)
  d <- data.frame(y=1)
  opt <- validate_options()
  # this should run normally
  expect_is(confront(v,d),'confrontation')
  expect_error(confront(v,d,raise='all'),info='Exectution time option overrules global options.')
  # the above statement should not yield side effects
  expect_is(confront(v,d),'confrontation')
  validate_options(v, raise='all')
  expect_error(confront(v,d))
  expect_is(confront(v,d,raise='none'), 'confrontation', info="Execution time option overwrites object option.")
  expect_error(confront(v,d),info='Execution time option has no side effects on object option.')
})


