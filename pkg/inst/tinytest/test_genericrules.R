
# numbers

expect_true(is_linear_sequence(numeric(0)))
expect_true( is_linear_sequence(0) )
expect_true( is_linear_sequence(c(0,1)) )
expect_false( is_linear_sequence(c(pi, exp(1),7)) )

expect_false( is_linear_sequence(c(3,4,2,1,5), sort=FALSE) )
expect_true( is_linear_sequence(c(3,4,2,1,5), sort=TRUE) )

expect_false( is_linear_sequence(c(1,NA,2)) )

expect_true( is_linear_sequence(NA_integer_) )
expect_false( is_linear_sequence(rep(NA_integer_,2)) )

expect_false( is_linear_sequence(1:5, start=2))
expect_false( is_linear_sequence(1:5, end=7))
expect_false( is_linear_sequence(1:5, start=1, end=6))
expect_false( is_linear_sequence(1:5, start=2, end=5))


# dates
expect_true( is_linear_sequence(as.Date("2015-12-17")) )
expect_true( is_linear_sequence( as.Date(c("2015-12-17","2015-12-19")) ) )
expect_false( is_linear_sequence(as.Date(c("2015-12-17","2015-12-19","2015-12-20"))) )

# POSIXct
expect_true( is_linear_sequence(as.POSIXct(as.Date("2015-12-17"))) )
expect_true( is_linear_sequence( as.POSIXct(as.Date(c("2015-12-17","2015-12-19"))) ) )
expect_false( is_linear_sequence(as.POSIXct(as.Date(c("2015-12-17","2015-12-19","2015-12-20"))) ) )

# in validator context

#d <- data.frame(
#    number = c(pi, exp(1), 7) 
#  , date = as.Date(c("2015-12-17","2015-12-19","2015-12-20"))
#  , time = as.POSIXct(as.Date(c("2015-12-17","2015-12-19","2015-12-20")))
#)
#
#rules <- validator(
#    is_linear_sequence(number)
#  , is_linear_sequence(date)
#  , is_linear_sequence(time)
#)
#
## nothing works 
#expect_false(any(confront(d,rules)))



