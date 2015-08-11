
context("Utilities")

test_that('Options can be set',{
  # warning on nonexistent option 
  expect_warning(validate_options(fiets=3))
  # invalid 'raise' value -- not implemented yet
  #expect_error(validate_options(raise='aap'))
  # this should run without problems
  validate_reset(validate_options)
  expect_equal(validate_options('raise')[[1]],'none')
})


test_that("match_cells",{
  d1 <- data.frame(id=paste(1:3),x=1:3,y=4:6)
  d2 <- data.frame(id=paste(4:1),y=4:7,x=1:4)
  expect_equal(
    names(match_cells(d1,d2,id='id')[[1]])
    ,names(match_cells(d1,d2,id='id')[[2]])    
  )
  expect_equal(
    as.character(match_cells(d1,d2,id='id')[[1]][,'id'])
    , as.character(match_cells(d1,d2,id='id')[[2]][,'id'])
  )  
})

test_that('validating/indicating expressions can be named',{
  expect_equal(names(validator(aap=x>3)),'aap')
  expect_equal(names(indicator(fiets=mean(x))),'fiets')    
})




# code for these methods in confrontation.R
test_that("other methods for 'variables'",{
  expect_equal(variables(women),c("height","weight"))  
  expect_equal(variables(as.list(women)),c("height","weight"))
  expect_equal(variables(as.environment(women)),c("height","weight"))
})


test_that('compare works',{
  d1 <- data.frame(x=1:3,y=4:6)
  d2 <- data.frame(x=c(NA,2,NA),y=c(4,5,NA))  
  v <- validator(x>0,y<5)
  a <- array(
    c(6,6,0,6,0,4,4,0,2,2,0
      ,6,3,3,3,0,2,2,0,1,1,0 ),dim=c(11,2)
  )
  expect_equivalent(unclass(compare(v,d1,d2)),a)  
})

test_that('blocks works',{
  v <- validator(x + y > z, q > 0, z + x == 3)
  expect_equivalent(v$blocks()[[1]],c(1,3))
  expect_equivalent(v$blocks()[[2]],2)
  v <- validator(
    x > 0
    , y > 0
    , x + y == z
    , u + v == w
    , u > 0)
  expect_equal(length(v$blocks()),2)
})



