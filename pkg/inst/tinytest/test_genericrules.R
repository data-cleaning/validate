
# numbers

expect_true( is_linear_sequence(numeric(0)))
expect_true( is_linear_sequence(0) )
expect_true( is_linear_sequence(c(0,1)) )
expect_false( is_linear_sequence(c(pi, exp(1),7)) )

expect_false( is_linear_sequence(c(3,4,2,1,5), sort=FALSE) )
expect_true( is_linear_sequence(c(3,4,2,1,5), sort=TRUE) )

expect_true( is.na(is_linear_sequence(c(1,NA,2))) )

expect_true( is_linear_sequence(NA_integer_) )
expect_true( is_linear_sequence(rep(NA_integer_,2)) )

expect_false( is_linear_sequence(1:5, start=2))
expect_false( is_linear_sequence(1:5, end=7))
expect_false( is_linear_sequence(1:5, start=1, end=6))
expect_false( is_linear_sequence(1:5, start=2, end=5))


# dates
expect_true( is_linear_sequence(as.Date("2015-12-17")) )
expect_true( is_linear_sequence( as.Date(c("2015-12-17","2015-12-19")) ) )
expect_false( is_linear_sequence(as.Date(c("2015-12-17","2015-12-19","2015-12-20"))) )

expect_true( 
  is_linear_sequence(
      as.Date(c("2015-12-17","2015-12-19","2015-12-21"))
    , start = as.Date("2015-12-17")
    , end   = as.Date("2015-12-21")
  ) )



# POSIXct
expect_true( is_linear_sequence( as.POSIXct("2015-12-17")) )
expect_true( is_linear_sequence( as.POSIXct(c("2015-12-17","2015-12-19")) ) )
expect_false( is_linear_sequence(as.POSIXct(c("2015-12-17","2015-12-19","2015-12-20")) ) )
# convesion of start/end?
expect_true( 
  is_linear_sequence( 
      as.POSIXct(c("2015-12-17","2015-12-19","2015-12-21")) 
    , start= as.POSIXct("2015-12-17")
    , end  = as.POSIXct("2015-12-21")              
  ) )



# character: auto-recognized formats
expect_true( is_linear_sequence(c("2012", "2013","2014")) )
expect_true( is_linear_sequence(c("2012M01", "2012M02", "2012M03")) )
expect_true( is_linear_sequence(c("2012Q1", "2012Q2", "2012Q3")) )
# conversion of start/end?
expect_true( is_linear_sequence(c("2012Q1", "2012Q2", "2012Q3"), start="2012Q1") )
expect_false( is_linear_sequence(c("2012Q1", "2012Q2", "2012Q3"), end="2012Q4") )



# in validator context

d <- data.frame(
    number = c(pi, exp(1), 7) 
  , date = as.Date(c("2015-12-17","2015-12-19","2015-12-20"))
  , time = as.POSIXct(as.Date(c("2015-12-17","2015-12-19","2015-12-20")))
)

rules <- validator(
    is_linear_sequence(number)
  , is_linear_sequence(date)
  , is_linear_sequence(time)
)

# nothing works 
expect_false(any(confront(d,rules)))


## Groupwise series in long format

dat <- data.frame(
    time = c(2012,2013,2012,2013,2015)
  , type = c("hi","hi","ha","ha","ha")
)

expect_false(all(check_that(dat, is_linear_sequence(time))))

expect_equivalent(
  values( check_that(dat, in_linear_sequence(time, type)) )[,1] 
  , c(TRUE,TRUE, FALSE, FALSE, FALSE)
)


# testing in_range

expect_true(in_range(1, min=0, max=1))
expect_false(in_range(1, min=0, max=1, strict=TRUE))

expect_true(in_range(as.Date("2018-03-01")
            , min=as.Date("2012-01-01")
            , max=as.Date("2018-03-01"))
            )
expect_false(in_range(as.Date("2018-03-01")
              , min=as.Date("2012-01-01")
              , max=as.Date("2018-03-01"), strict=TRUE)
            )















