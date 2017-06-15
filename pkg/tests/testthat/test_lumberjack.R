
#devtools::load_all('pkg')

context("lumberjack loggers")
test_that("lbj_cells",{
  fl <- tempfile()
  lbj <- lbj_cells()
  meta <- list(expr = expression(foo()), src = "foo()")
  w1 <- women
  w1[1,1] <- 2*w1[1,1]
  lbj$add(meta,women,w1)
  lbj$cells
  lbj$dump(file=fl)
  d <- read.csv(fl)
  expect_equal(nrow(d),2)
  expect_equal(d$adapted,c(0,1))
  expect_warning( lbj$add(meta, d, data.frame(x=1:2)) )
})

test_that("lbj_rules",{
  fl <- tempfile()
  v <- validator(x>0)
  lbj <- lbj_rules(rules=v)
  meta <- list(expr = expression(foo()), src = "foo()")
  d1 <- data.frame(x =-1)
  d2 <- data.frame(x = 1)
  lbj$add(meta,d1,d2)
  lbj$dump(file=fl)
  d <- read.csv(fl)
  expect_equal(nrow(d),2)
  expect_equal(d$satisfied,c(0,1))
  expect_warning( lbj$add(meta,d1,data.frame(x=1:2)) )
})