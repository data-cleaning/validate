
context("derive linear coefficients")

test_that("linear coeffiencts can be derived",{
  v <- validator(x>0)
  expect_equivalent(v$linear_coefficients()$A , matrix(-1,1,1))
  expect_equivalent(v$linear_coefficients()$b, matrix(0,1,1))
  w <- validator(2*x > 0)
  expect_equivalent(w$linear_coefficients()$A,matrix(-2,1,1))
  expect_equivalent(w$linear_coefficients()$b,matrix(0,1,1))
  x <- validator(2*x + y > 0)
  expect_equivalent(x$linear_coefficients()$A,matrix(c(-2,-1),1,2))
  expect_equivalent(x$linear_coefficients()$b,matrix(0,1,1))  
  y <- validator(2*x + y > 3*z + w)  
  expect_equivalent(y$linear_coefficients()$A,matrix(c(-2,-1,3,1),1,4))
  expect_equivalent(y$linear_coefficients()$b,matrix(0,1,1))  
  z <- validator(2*x + 1 +  y -2 > 4 + z - 8)
  expect_equivalent(z$linear_coefficients()$A,matrix(c(-2,-1,1),1,3))
  expect_equivalent(z$linear_coefficients()$b,matrix(3,1,1))
})

test_that("normalisation can be switched off",{
  # check normalisation
  z <- validator(2*x + 1 +  y -2 > 4 + z - 8)
  expect_equivalent(z$linear_coefficients(normalize=FALSE)$A, -1*matrix(c(-2,-1,1),1,3))
  expect_equivalent(z$linear_coefficients(normalize=FALSE)$b, -1*matrix(3,1,1))    
})

