
context('Options')

test_that("Options can be set and reset locally",{
  v <- validator()
  voptions(v, raise='all')
  expect_false(voptions()$raise == voptions(v)$raise, info="local option set")
  reset(v)
  expect_equal(voptions(v,'raise'),"none",info="local option reset")
})

test_that("Options can be executed locally without side effects",{
  v <- validator(x > 0)
  d <- data.frame(y=1)
  opt <- voptions()
  # this should run normally
  expect_is(confront(d,v),'confrontation')
  expect_error(confront(d,v,raise='all'),info='Exectution time option overrules global options.')
  # the above statement should not yield side effects
  expect_is(confront(d,v),'confrontation')
  voptions(v, raise='all')
  expect_error(confront(d,v))
  expect_is(confront(d,v,raise='none'), 'confrontation', info="Execution time option overwrites object option.")
  expect_error(confront(d,v),info='Execution time option has no side effects on object option.')
})


