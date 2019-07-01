


## validation syntax is recognized ----
  # fiets(x) is not a validation rule
  expect_warning(validator(fiets(x)))

  # these are not validating statements
  expect_warning(validator(x?y))
  expect_warning(validator(1>0))


## Exception handling can be switched 
  
  voptions(raise='none')
  expect_equal(validate:::factory(function()stop('aap'), voptions)()$err, 'aap')
  expect_equal(validate:::factory(function(){ warning('aap');7}, voptions)()$warn, 'aap')
  
  voptions(raise='errors')
  expect_error(validate:::factory(function() stop(), voptions)())
  
  voptions(raise = 'all')
  expect_error(validate:::factory(function() stop(),voptions)())
  expect_warning(validate:::factory(function() warning(),voptions)())
  
  voptions('reset')




## Functional dependencies
  v1 <- validator(stad + straat ~ postcode)
  dat <- data.frame(
    straat = c('kerkstraat','kerkstraat','kerkstraat','kerkstraat')
    ,stad = c('DH','DH','H','DH')
    ,postcode = c('2495','2496','8888','2495')
  )
  cf <- confront(dat,v1)
  expect_equivalent(values(cf),array(c(TRUE,FALSE,TRUE,TRUE),dim=c(4,1)))


## group_expansion ----
  L <- list(expression(var_group(a,b)>0)[[1]])
  expect_equal(length(validate:::expand_groups(L)),2)
  # one expression not containing group
  L <- list(expression(var_group(a,b)>0)[[1]],expression(x>0)[[1]])
  expect_equal(length(validate:::expand_groups(L)),3)
  # two groups (cartesian product)
  L <- list(expression(var_group(a,b)>var_group(b,c))[[1]])
  expect_equal(length(validate:::expand_groups(L)),4)





