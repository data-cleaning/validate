
context("derive linear coefficients")

test_that("linear coeffiencts can be derived",{
  v <- validator(x>0)
  expect_equivalent(linear_coefficients(v)$A , matrix(-1,1,1))
  expect_equivalent(linear_coefficients(v)$b, matrix(0,1,1))
  w <- validator(2*x > 0)
  expect_equivalent(linear_coefficients(w)$A,matrix(-2,1,1))
  expect_equivalent(linear_coefficients(w)$b,matrix(0,1,1))
  x <- validator(2*x + y > 0)
  expect_equivalent(linear_coefficients(x)$A,matrix(c(-2,-1),1,2))
  expect_equivalent(linear_coefficients(x)$b,matrix(0,1,1))  
  y <- validator(2*x + y > 3*z + w)  
  expect_equivalent(linear_coefficients(y)$A,matrix(c(-2,-1,3,1),1,4))
  expect_equivalent(linear_coefficients(x)$b,matrix(0,1,1))  
  z <- validator(2*x + 1 +  y -2 > 4 + z - 8)
  expect_equivalent(linear_coefficients(z)$A,matrix(c(-2,-1,1),1,3))
  expect_equivalent(linear_coefficients(z)$b,matrix(3,1,1))
})

test_that("normalisation can be switched off",{
  # check normalisation
  z <- validator(2*x + 1 +  y -2 > 4 + z - 8)
  expect_equivalent(linear_coefficients(z,normalize=FALSE)$A, -1*matrix(c(-2,-1,1),1,3))
  expect_equivalent(linear_coefficients(z,normalize=FALSE)$b, -1*matrix(3,1,1))    
})

