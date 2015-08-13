

context("validator object")

test_that("setting properties",{
  v <- validator(x>0,y+x==1)
  names(v)[1] <- "foo"
  expect_equal(names(v),c("foo","V2"))
  origin(v)[1] <- "faa"
  expect_equal(origin(v),c("faa","command-line"))
  label(v)[1] <- "fee"
  expect_equal(label(v),c("fee",""))
  description(v)[1] <- "foobar"
  expect_equal(description(v),c("foobar",""))

  # a name, label, origin or description should be a single 'character' element 
  expect_warning(origin(v)[1] <- c("fu","bar"))
  expect_warning(names(v)[1] <- c("fu","bar"))
  expect_warning(label(v)[1] <- c("fu","bar"))
  expect_warning(description(v)[1] <- c("fu","bar"))
   
})

 
 