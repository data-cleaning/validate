
# numbers
expect_true( is_sequence(0) )
expect_true( is_sequence(c(0,1)) )
expect_false( is_sequence(c(pi, exp(1),7)) )

# dates
expect_true( is_sequence(as.Date("2015-12-17")) )
expect_true( is_sequence( as.Date(c("2015-12-17","2015-12-19")) ) )
expect_false( is_sequence(as.Date(c("2015-12-17","2015-12-19","2015-12-20"))) )

# POSIXct
expect_true( is_sequence(as.POSIXct(as.Date("2015-12-17"))) )
expect_true( is_sequence( as.POSIXct(as.Date(c("2015-12-17","2015-12-19"))) ) )
expect_false( is_sequence(as.POSIXct(as.Date(c("2015-12-17","2015-12-19","2015-12-20"))) ) )

# in validator context

d <- data.frame(
    number = c(pi, exp(1), 7) 
  , date = as.Date(c("2015-12-17","2015-12-19","2015-12-20"))
  , time = as.POSIXct(as.Date(c("2015-12-17","2015-12-19","2015-12-20")))
)

rules <- validator(
    is_sequence(number)
  , is_sequence(date)
  , is_sequence(time)
)

# nothing works 
expect_false(any(confront(d,rules)))



