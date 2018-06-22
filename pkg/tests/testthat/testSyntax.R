
context("Syntax attributes")

test_that("validation syntax is recognized",{
  # fiets(x) is not a validation rule
  expect_warning(validator(fiets(x)))

  # these are not validating statements
  expect_warning(validator(x?y))
  expect_warning(validator(1>0))
})


test_that('Exception handling can be switched',{
  
  voptions(raise='none')
  expect_equal(factory(function()stop('aap'), voptions)()$err, 'aap')
  expect_equal(factory(function(){ warning('aap');7}, voptions)()$warn, 'aap')
  
  voptions(raise='errors')
  expect_error(factory(function() stop(), voptions)())
  
  voptions(raise = 'all')
  expect_error(factory(function() stop(),voptions)())
  expect_warning(factory(function() warning(),voptions)())
  
  voptions('reset')
})




test_that("Functional dependencies", {
  v1 <- validator(stad + straat ~ postcode)
  dat <- data.frame(
    straat = c('kerkstraat','kerkstraat','kerkstraat','kerkstraat')
    ,stad = c('DH','DH','H','DH')
    ,postcode = c('2495','2496','8888','2495')
  )
  cf <- confront(dat,v1)
  expect_equivalent(values(cf),array(c(TRUE,FALSE,TRUE,TRUE),dim=c(4,1)))
})

test_that("group_expansion",{
  L <- list(expression(var_group(a,b)>0)[[1]])
  expect_equal(length(expand_groups(L)),2)
  # one expression not containing group
  L <- list(expression(var_group(a,b)>0)[[1]],expression(x>0)[[1]])
  expect_equal(length(expand_groups(L)),3)
  # two groups (cartesian product)
  L <- list(expression(var_group(a,b)>var_group(b,c))[[1]])
  expect_equal(length(expand_groups(L)),4)
})





