
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


test_that('Missings are counted correctly',{
  d1 <- data.frame(x=1:3,y=4:6) 
  d2 <- data.frame(x=c(NA,2,NA),y=c(4,5,NA))
  expect_equal(evalq(number_missing(),d1),    0L)
  expect_equal(evalq(number_missing(),d2),    3L)
  expect_equal(evalq(number_missing(x),d2),   2L)
  expect_equal(evalq(number_missing(x,y),d2), 3L)
#  expect_equal(evalq(number_missing("."),d2), 3L)  
  
  expect_equal(evalq(fraction_missing(),d1),    0/6)
  expect_equal(evalq(fraction_missing(),d2),    3/6)
  expect_equal(evalq(fraction_missing(x),d2),   2/3)
  expect_equal(evalq(fraction_missing(x,y),d2), 3/6)
#  expect_equal(evalq(fraction_missing("."),d2), 3/6)  
  
  
  expect_equal(evalq(row_missing(),d1),    c(0L,0L,0L))
  expect_equal(evalq(row_missing(),d2),    c(1L,0L,2L))
  expect_equal(evalq(row_missing(x),d2),   c(1L,0L,1L))
  expect_equal(evalq(row_missing(x,y),d2), c(1L,0L,2L))
#  expect_equal(evalq(row_missing("."),d2), c(1L,0L,2L))  
  
  expect_equal(evalq(col_missing(),d1),    c(x=0L,y=0L))
  expect_equal(evalq(col_missing(),d2),    c(x=2L,y=1L))
  expect_equal(evalq(col_missing(x),d2),   c(x=2L)     )
  expect_equal(evalq(col_missing(x,y),d2), c(x=2L,y=1L))
#  expect_equal(evalq(col_missing("."),d2), c(x=2L,y=1L))  

  expect_equal(evalq(number_unique(),d1),3)
  expect_equal(evalq(any_duplicated(),d1),FALSE)
  
    
  expect_equal(evalq(any_missing(),d1), FALSE)
#  expect_equal(evalq(any_missing("."),d1), FALSE)
  expect_equal(evalq(any_missing(),d2), TRUE)
#  expect_equal(evalq(any_missing("."),d2), TRUE)
  expect_equal(evalq(any_missing(x),d2), TRUE)
  expect_equal(evalq(any_missing(x,y),d2), TRUE)
  
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





