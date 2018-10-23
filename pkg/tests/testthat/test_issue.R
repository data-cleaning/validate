context("Github issues")

test_that("issue #91 is solved",{
  data <- data.frame(A = 1)
  rule <- validator(A > 0)
  cf <- confront(data, rule)
  plot(rule)
  plot(cf)
})

