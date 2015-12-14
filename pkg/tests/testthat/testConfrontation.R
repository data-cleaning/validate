
context("confrontation")



test_that("validation object contents",{
  cf <- check_that(women, height > 0, ape > 0,weight / height > 2 )  
  expect_equal(length(cf),3)
  expect_equal(names(summary(cf)),c("rule","items","passes","fails","nNA","error","warning","expression"))
  vl <- rep(TRUE,30)
  vl[16:18] <- FALSE
  expect_equivalent(values(cf), array(vl,dim=c(15,2)))
  expect_equal(errors(cf)[[1]],"object 'ape' not found")
  expect_equivalent(warnings(cf),list())
  agg <- data.frame(
    npass = c(V1=15,V3=12)
    , nfail = c(0,3)
    , nNA = c(0,0)
    , rel.pass = c(1.0,0.8)
    , rel.fail = c(0.0,0.2)
    , rel.NA = c(0,0)
  )
  expect_equal(aggregate(cf),agg)
  expect_equal(sort(cf),agg[2:1,])
  expect_equivalent(class(cf[1]),"validation")  
  expect_equal(length(cf[1]),1)
})

test_that("indication object contents",{
  ind <- indicator(mean(height),sd(weight), sum(foo))
  cf <- confront(women, ind)
  expect_equal(length(cf),3)
  expect_equivalent(round(values(cf),3),array(c(65,15.499),dim=c(1,2)))  
  expect_equal(errors(cf)[[1]],"object 'foo' not found")
  expect_equal(names(summary(cf)),
    c("indicator"  
      ,"items"
      ,"class"      
      ,"min"
      ,"mean" 
      ,"max"
      ,"nNA"
      ,"error"
      ,"warning"
      ,"expression")
  )  
  expect_equal(dim(summary(cf)),c(3,10))
})


test_that("confrontation method with custom na.values",{
  
  v <- validator(x > 0)
  d <- data.frame(x=c(1,-1,NA))
  expect_equivalent(values(confront(d,v)), matrix(c(TRUE,FALSE,NA)) )
  expect_equivalent(values(confront(d,v,na.value=FALSE)), matrix(c(TRUE,FALSE,FALSE)) )
  expect_equivalent(values(confront(d,v,na.value=TRUE)), matrix(c(TRUE,FALSE,TRUE)) )
  
})

test_that("Confrontation methods with reference data",{
  v1 <- validator(height > 0, weight / height > 0, height == ref$height)
  cf1 <- confront(women,v1,ref = women)
  v2 <- validator(height > 0, weight / height > 0, height == w1$height)
  cf2 <- confront(women,v2,ref=list(w1=women))
  e <- new.env()
  e$w1 <- women
  cf3 <- confront(women, v2, ref=e)
  expect_equal(summary(cf1)[1:7],summary(cf2)[1:7])
  expect_equal(summary(cf2)[1:7],summary(cf3)[1:7])
  
  # warning when the data-carrying environment has variables with the
  # same name as the parent.
  
     expect_warning(confront(
      dat   = data.frame(test=10)
      , x   = validator(test==test$aap)
      , ref = list(test=data.frame(aap=7)))
     )
})



test_that("confrontations with transient variables",{
  v <- validator(rat := weight/height, rat >0)
  expect_equivalent(values(confront(women,v)), array(TRUE,dim=c(15,1)))
    
})

test_that("check_that works with simple example",{
  dat <- data.frame(x=1:2, y=3:2)
  cf <- check_that(dat, x >= y)
})

test_that("Confrontations with slack on linear equalities",{
  v <- validator(x == 10)
  d <- data.frame(x=9)
  expect_false(values(confront(d,v)))
  expect_true(values(confront(d,v,lin.eq.eps=2)))
  # setting slack on equalities should not matter for inequalities
  w <- validator(x > 10)
  expect_false(values(confront(d,w)))
  expect_false(values(confront(d,w,lin.eq.eps=2)))
  # should also work in linear subexpressions
  u <- validator( if (x == 10) y > 0)
  d <- data.frame(x=9,y=-1)
  expect_true(values(confront(d,u)))
  expect_false(values(confront(d,u,lin.eq.eps=2)))
})



