
# a number of methods not otherwise tested in testOptions or testParse

context("Methods for classes (inheriting from) expressionset")

test_that("Expressionset extraction",{
  v <- validator(x > 0, y>0)
  expect_equivalent(class(v[[1]]),"rule")
  expect_equivalent(class(v[["V1"]]),"rule")
  expect_equivalent(class(v[1]),"validator")
  expect_equal(length(v[1]),1)
  expect_equal(length(v[1:2]),2)
  expect_equivalent(class(summary(v)),"data.frame")
  expect_true(all(c("block","nvar","rules") %in% names(summary(v))) )
})


test_that("Variables can be retrieved",{
  expect_equal( variables(validator(x > 0)),'x')
  expect_equal( sort(variables(validator(x > 0, y > 0))) , c('x','y') ) 
  expect_equal( variables(validator(x>0, x<1 )), 'x')
  expect_equal( sort(variables(validator(x +y > 0, y < 1))), c('x','y') )
  expect_equal( variables(validator(x := 2*y, x>1)),'y')
  expect_equal( sort(variables(validator(x := 2*y, x>1),dummy=TRUE)), c('x','y'))  
  v <- validator(
    root = y := sqrt(x)
   , average = mean(x) > 3
   , sum = x + y == z
  )
  expect_equivalent(
    variables(v,as='matrix')
  , array(c(T,T,F,T),dim=c(2,2))
  )
  expect_equivalent(
    variables(v,as='matrix',dummy=TRUE)
  , array(c(T,F,T,T,T,T,F,F,T),dim=c(3,3))
  )
})



context("Methods for classes (inheriting from) confrontation")
test_that("Confrontation extraction",{
  cf <- check_that(women,height > weight, height > 0)  
  expect_equal(length(cf),2)
  expect_equal(length(cf[1]),1)
})




