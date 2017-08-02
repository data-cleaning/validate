
# a number of methods not otherwise tested in testOptions or testParse

context("Methods for classes (inheriting from) expressionset")

test_that("Expressionset extraction",{
  v <- validator(x > 0, y>0)
  expect_equivalent(class(v[[1]]),"rule")
  expect_equivalent(class(v[["V1"]]),"rule")
  expect_equivalent(class(v[1]),"validator")
  expect_equal(length(v[1]),1)
  expect_equal(length(v[1:2]),2)
  expect_equal(length(v["V1"]),1)
  expect_equal(length(v[c("V1","V2")]),2)
  expect_equivalent(class(summary(v)),"data.frame")
  expect_true(all(c("block","nvar","rules") %in% names(summary(v))) )
})

test_that("name setter",{
  v <- validator(x>0,y>0,z>0)
  expect_warning(names(v) <- c("A","B"))
  expect_true(!any(duplicated(names(v))))
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
  v <- validator(x + y > 0, z>0)
  expect_equal(sort(variables(v[[1]])), c('x','y'))
  
  # test reuse of dummy variables to define other dummies.
  # this also tests expand_assignments
  v <- validator( dummy_x:=1,  dummy_y:= dummy_x + 1, z > dummy_y)
  expect_equal(variables(v, dummy=FALSE),"z")
  
})



context("Methods for classes (inheriting from) confrontation")
test_that("Confrontation extraction",{
  cf <- check_that(women,height > weight, height > 0)  
  expect_equal(length(cf),2)
  expect_equal(length(cf[1]),1)
})

# just a simple test to check consistency between barplot and confrontation objects.
test_that("barplot doesn't crash",{
  nullplot <- function(...){
    pdf(NULL)
    on.exit(dev.off())
    barplot(check_that(women, height>0, weight/height > 2),...)
  }
  nullplot()
  nullplot(add_exprs=TRUE)
  nullplot(add_legend=FALSE)
  nullplot(topn=5)
})


test_that("show methods do not crash",{
  x <- capture.output(validator(x + y == z))
  x <- capture.output(validator(x + y == z)[[1]])
  x <- capture.output(check_that(women,height>0))
})

test_that("yaml export",{
  # smoke test
  as_yaml(validator(x>0))
  export_yaml(x=validator(x>0), file=tempfile())
  # test that options are included, only when provided
  v <- validator(x>0)
  expect_false(grepl("options:",as_yaml(v)))
  voptions(v,raise="all")
  expect_true(grepl("options:",as_yaml(v)))
})


