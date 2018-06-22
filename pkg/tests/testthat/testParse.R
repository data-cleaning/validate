context("Parsing files")

test_that("file paths are interpreted correctly",{
  expect_true(is_full_path("C:/hello"))
  expect_true(is_full_path("//server/hello"))
  expect_true(is_full_path("~/hello"))
  expect_true(is_full_path("http://hello"))
  expect_false(is_full_path("./hello"))
  expect_false(is_full_path("reldir/hello"))
  # windoze flavor
  expect_true(is_full_path("C:\\hello"))
  expect_true(is_full_path("\\\\server\\hello"))
  expect_true(is_full_path("~\\hello"))
  expect_false(is_full_path("reldir\\hello"))
})

# TODO: add file parsing tests
#setwd("pkg/tests/testthat/")
test_that("Parsing freeform", {
  expect_equal( length( validator(.file="yamltests/freeform.yaml") ) , 2,info="freeform")
  expect_equal( length( indicator(.file="yamltests/indicator.yaml") ) , 2,info="freeform")
  expect_equal( length( indicator(.file="yamltests/indicator2.yaml") ) , 2,info="freeform")
})

test_that("Parsing yrf format", {
  now <- Sys.time()
  v <- validator(.file="yamltests/yamlrules.yaml")
  expect_equal(length(v),2)
  expect_equal(names(v),c("sumrule","conditional"))
  expect_equivalent(origin(v),c("yamltests/yamlrules.yaml","yamltests/yamlrules.yaml"))
  expect_equivalent(label(v),c("sum of x and y","if x positive then y also"))
  expect_equivalent(description(v),c("a looong description here","a looong description here\n"))
  expect_true(all(created(v)-now < 10))
  expect_warning(validator(.file="yamltests/invalid.yaml"))
  out <- capture.output(expect_warning(validator(.file="yamltests/invalidR.yaml")))
})

test_that("Parsing options",{
  v <- validator(.file="yamltests/yamloptions.yaml") 
  expect_equal(voptions(v,"raise"),"all")
  expect_equal(length(v),1)
})

test_that("Parsing metadata",{
  v <- validator(.file="yamltests/yaml_with_meta.yaml")
  expect_equal(meta(v)$foo,c("1",NA))
  expect_equal(meta(v)$bar,c(NA,"2"))
})

test_that("Parsing included files",{
  v <- validator(.file="yamltests/top.yaml")
  expect_equal(length(v),6)
  expect_equivalent(origin(v)
    , c(  "yamltests/child1.yaml"
        , "yamltests/child1.yaml"
        , "yamltests/child3.yaml"
        , "yamltests/child2.yaml"
        , "yamltests/child2.yaml"
        , "yamltests/top.yaml")
    , info = "file inclusion order"
    )
})

test_that("validation from data.frames",{
  
  d <- data.frame(
  rule = c("x>0", "a + b == c")
    , name = c("foo", "bar")
    , description = c("hello world","Ola, mundo")
    , stringsAsFactors=FALSE
  )
  expect_equal(length(validator(.data=d)),2)
  expect_equal(length( validator(.data=d[-3]) ),2)
  expect_error(validator(.data=d[-1]))
  d$rule[2] <- "a+b"
  expect_warning(validator(.data=d))
  
})


context("Computing on language")
# 
test_that("var_from_call",{
  
  # regular case, concering two variables
  expect_equal(
    var_from_call(expression(x > y)[[1]])
    , c("x","y")
  )
  
  
  # case of no variables at all
  expect_equal(
    var_from_call(expression(1 > 0)[[1]])
    , NULL
  )
})

test_that("validating_call",{
  expect_true(validating_call(expression(x > y)[[1]]))
  expect_true(validating_call(expression(x >= y)[[1]]))
  expect_true(validating_call(expression(x == y)[[1]]))
  expect_true(validating_call(expression(x != y)[[1]]))
  expect_true(validating_call(expression(x <= y)[[1]]))
  expect_true(validating_call(expression(x < y)[[1]]))
  expect_true(validating_call(expression(identical(x,y))[[1]]))

  expect_true(validating_call(expression(!(x > y))[[1]]))
  expect_true(validating_call(expression(all(x > y))[[1]]))
  expect_true(validating_call(expression(any(x > y))[[1]]))
  expect_true(validating_call(expression(grepl('hello',x))[[1]]))

  expect_true(validating_call(expression(if(x == 1) y == 1)[[1]]))
  expect_true(validating_call(expression(xor(x == 1, y == 1))[[1]]))
  expect_false(validating_call(expression(x)[[1]]))
  
})

test_that("vectorizing if-statmentes",{

  a <- vectorize( expression( if (P) Q )[[1]]  )
  b <- expression(!(P) |(Q))[[1]]
  expect_identical(a,b)

  a <- vectorize( expression( (if (P) Q) )[[1]]  )
  b <- expression( (!(P)|(Q)) )[[1]]  
  expect_identical(a,b)

  a <- vectorize( expression( (if (P) Q) | Z   )[[1]]  )
  b <- expression((!(P)|(Q)) | Z)[[1]]
  expect_identical(a,b)

  a <- expression(sapply(x,function(y) 2*y))[[1]]
  b <- a
  expect_identical(vectorize(a),b)

  a <- vectorize( expression( (if (P) Q) | (if(A) B)   )[[1]]  )
  b <- expression((!(P)|(Q))|(!(A)|(B)))[[1]]
  expect_identical(a,b)

  # nested if's. For some reasons, identical gives FALSE
  a <- vectorize(expression( if (P) Q | if(A) B )[[1]])
  b <- expression( !(P) | (Q | (!(A) | (B))) )[[1]]
  expect_true(a == b)
  
  e <- expression( if (P) Q else R)[[1]]
  a <- vectorize(e)
  b <- expression(
    (!(P)|(Q)) & ((P)|(R))
  )[[1]]
  expect_identical(a,b)
})


test_that("translation of rules to data.frame",{
  v <- validator(x > y, 2*y-1==z)
  expect_equal(nrow(as.data.frame(v)),2)
  i <- indicator(mean(x), sd(y))
  expect_equal(nrow(as.data.frame(i)),2)
})


test_that("replacing %in% operator",{
  e <- expression( x %in% y)[[1]]
  expect_identical(replace_in(e)
    , expression(x %vin% y)[[1]])
  
  e <- expression( x %in% y | x %in% z)[[1]]
  expect_identical(replace_in(e)
    , expression(x %vin% y | x %vin% z)[[1]])
                   
  
})
