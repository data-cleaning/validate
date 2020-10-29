
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

expect_false( is_linear_sequence(1:5, begin=2))
expect_false( is_linear_sequence(1:5, end=7))
expect_false( is_linear_sequence(1:5, begin=1, end=6))
expect_false( is_linear_sequence(1:5, begin=2, end=5))


# dates
expect_true( is_linear_sequence(as.Date("2015-12-17")) )
expect_true( is_linear_sequence( as.Date(c("2015-12-17","2015-12-19")) ) )
expect_false( is_linear_sequence(as.Date(c("2015-12-17","2015-12-19","2015-12-20"))) )

expect_true( 
  is_linear_sequence(
      as.Date(c("2015-12-17","2015-12-19","2015-12-21"))
    , begin = as.Date("2015-12-17")
    , end   = as.Date("2015-12-21")
  ) )


expect_true(is_linear_sequence(rep(1:5, each=2), by=rep(letters[1:2],5)))


# POSIXct
expect_true( is_linear_sequence( as.POSIXct("2015-12-17")) )
expect_true( is_linear_sequence( as.POSIXct(c("2015-12-17","2015-12-19")) ) )
expect_false( is_linear_sequence(as.POSIXct(c("2015-12-17","2015-12-19","2015-12-20")) ) )
# convesion of start/end?
expect_true( 
  is_linear_sequence( 
      as.POSIXct(c("2015-12-17","2015-12-19","2015-12-21")) 
    , begin= as.POSIXct("2015-12-17")
    , end  = as.POSIXct("2015-12-21")              
  ) )



# character: auto-recognized formats
expect_true( is_linear_sequence(c("2012", "2013","2014")) )
expect_true( is_linear_sequence(c("2012M01", "2012M02", "2012M03")) )
expect_true( is_linear_sequence(c("2012Q1", "2012Q2", "2012Q3")) )
# conversion of start/end?
expect_true( is_linear_sequence(c("2012Q1", "2012Q2", "2012Q3"), begin="2012Q1") )
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

# testing part-whole relation checks

labels <- c("2018Q1", "2018Q2", "2018Q3", "2018Q4","2018")
values <- c(1,2,3,4, 10)

expect_equal(
  part_whole_relation(values, labels, whole=rx("^\\d{4}$"))
 , rep(TRUE, 5)
)

values[1] <- 2

expect_equal(
  part_whole_relation(values, labels, whole=rx("^\\d{4}$"))
 , rep(FALSE, 5)
)

values <- rep(values, 2)
values[1] <- 1
labels <- rep(labels, 2)
direction <- rep(c("import", "export"), each=5)

expect_equal(
  part_whole_relation(values, labels, whole=rx("^\\d{4}$"), by=direction)
  , c(rep(TRUE, 5), rep(FALSE, 5))
)

values[1] <- NA

expect_equal(
  part_whole_relation(values, labels, whole=rx("^\\d{4}$"), by=direction)
  , c(rep(NA, 5), rep(FALSE, 5))
)

expect_equal(
  part_whole_relation(values, labels, whole=rx("^\\d{4}$"), by=direction, na.rm=TRUE)
  , c(rep(FALSE, 5), rep(FALSE, 5))
)

# with string literals
local({
  region <- c("foo", "bar","baz","bur","boo","fu")
  amount <- c(10, 4:1, 25)
  expect_equal(
    part_whole_relation(amount, region, whole="foo", part=c("bar","bur","baz","boo"))
    , rep(TRUE, length(region))
  )
})

## testing do_by

x <- 1:10
y <- rep(letters[1:2],5)

expect_equal(do_by(x,y,sum), rep(c(25,30), 5))
x[1] <- NA
expect_equal(do_by(x,y,max), rep(c(NA,10),5))


expect_equal(sum_by(c(1,2),letters[1:2]), c(1,2))
expect_equal(min_by(c(1,2),letters[1:2]), c(1,2))
expect_equal(max_by(c(1,2),letters[1:2]), c(1,2))
expect_equal(mean_by(c(1,2),letters[1:2]), c(1,2))

# field lenght
expect_true(field_length("abc",3))
expect_false(field_length("abc",2))
expect_true(field_length("abc",min=1, max=3))

## number format 

expect_true(number_format("12.34","dd.dd"))
expect_false(number_format("12.345","dd.dd"))
expect_true(number_format("0.123E45","0.d*Edd"))
expect_false(number_format("0.12x", "0.d*"))
expect_true(number_format("0.12x", "0.d*x"))

expect_true(number_format("12.34",min_dig=0))
expect_true(number_format("12.34",min_dig=1))
expect_true(number_format("12.34",min_dig=2))
expect_false(number_format("12.34",min_dig=3))
expect_true(number_format("12.34",max_dig=3))
expect_true(number_format("12.34",max_dig=2))
expect_false(number_format("12.34",max_dig=1))
expect_true(number_format("12.34",min_dig=1,max_dig=2))
expect_false(number_format("12.34",min_dig=3,max_dig=5))
expect_true(number_format("12,34",min_dig=1,max_dig=2, dec=","))


## Checking data against a fixed set of key-combinations

dat <- data.frame(
    year    = rep(c("2018","2019"),each=4)
  , quarter = rep(sprintf("Q%d",1:4), 2)
  , value   = sample(20:50,8)
)

# explicit case
rule <- validator(contains_exactly(
           expand.grid(year=c("2018","2019"), quarter=c("Q1","Q2","Q3","Q4"))
          )
        )
expect_equivalent(values(confront(dat, rule)), matrix(TRUE,nrow=8))

# cases using a reference keyset
keyset  <- expand.grid(year=c("2018","2019"), quarter=c("Q1","Q2","Q3","Q4"))
keyset1 <- keyset[-1,]
 
rule  <- validator(contains_exactly(all_keys))

expect_equivalent( as.logical(values(confront(dat, rule, ref=list(all_keys = keyset)))), rep(TRUE,8) )
expect_equivalent( as.logical(values(confront(dat, rule, ref=list(all_keys = keyset1)))), rep(FALSE,8))


dat1 <- dat[-1,]
rule <- validator(contains_at_most(all_keys))
expect_equivalent(
  as.logical(values(confront(dat, rule, ref=list(all_keys = keyset))))
  , rep(TRUE,8))
expect_equivalent( 
  as.logical(values(confront(dat, rule, ref=list(all_keys = keyset1)))) 
  , c(FALSE, rep(TRUE,7))
  )

rule <- validator(contains_at_least(all_keys))
expect_true(all(confront(dat, rule, ref=list(all_keys=keyset))))
expect_false(all(confront(dat1, rule, ref=list(all_keys=keyset))))

rule <- validator(does_not_contain(forbidden_keys))

expect_equivalent(
  as.logical(values(confront(dat, rule, ref=list(forbidden_keys=keyset))))
 , rep(FALSE, 8))


## Globbing and Regex ---------------------------------------------------------

transactions <- data.frame(
  sender   = c("S1","S2", "S3", "R1")
, receiver = c("R1","S1", "R1", "S1")
)

# a sender 'S*' cannot send to a sender
rule <- validator(does_not_contain(glob(data.frame(sender = "S*", receiver="S*"))))
expect_equal(as.logical(values(confront(transactions, rule))), c(TRUE, FALSE, TRUE, TRUE)
    ,info="globbing in does_not_contain" )


rule <- validator(does_not_contain(rx(data.frame(sender = "^S", receiver="^S"))))
expect_equal(as.logical(values(confront(transactions, rule))), c(TRUE, FALSE, TRUE, TRUE)
    ,info="regex in does_not_contain" )

# sender ending with a 2 cannot send to receiver ending with 1
rule <- validator(does_not_contain(rx(data.frame(sender = "2$", receiver="1$"))))
expect_equal(as.logical(values(confront(transactions, rule))), c(TRUE, FALSE, TRUE, TRUE)
    ,info="regex in does_not_contain" )



## Grouping -------------------------------------------------------------------

# data in 'long' format
dat <- expand.grid(
  year = c("2018","2019")
  , quarter = c("Q1","Q2","Q3","Q4")
  , variable = c("import","export")
)
dat$value <- sample(50:100,nrow(dat))


periods <- expand.grid(
  year = c("2018","2019")
  , quarter = c("Q1","Q2","Q3","Q4")
)

rule <- validator(contains_exactly(all_periods, by=variable))

out <- confront(dat, rule, ref=list(all_periods=periods))
expect_equivalent(as.logical(values(out)), rep(TRUE,nrow(dat)))

# remove one  export record

dat1 <- dat[-15,]
out1 <- confront(dat1, rule, ref=list(all_periods=periods))
values(out1)
expect_equivalent(as.logical(values(out1)), c(rep(TRUE,8),rep(FALSE, 7)) )

## Field format

expect_equal(field_format(c("X0Y","X12"), "^X\\dY",type="regex"), c(TRUE, FALSE))
expect_equal(field_format(c("X0Y","Y12"), "X*",type="glob"), c(TRUE, FALSE))


## hierarchy ------------------------------------------------------------------
#
d <- data.frame(
      nace   = c("01","01.1","01.11","01.12", "01.2")
    , volume = c(100 ,70    , 30    ,40     , 25)
)
data(nace_rev2)
expect_equal(hierarchy(d$volume, labels=d$nace, hierarchy=nace_rev2[3:4])
            , c(FALSE, FALSE, TRUE, TRUE, FALSE))


d <- data.frame(
      nace   = c("01","01.1","01.11","01.12", "01.2","foo")
    , volume = c(100 ,70    , 30    ,40     , 25    , 60)
)
expect_equal(hierarchy(d$volume, labels=d$nace, hierarchy=nace_rev2[3:4])
            , c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))

expect_equal(hierarchy(d$volume, labels=d$nace, hierarchy=nace_rev2[3:4], na_value=NA)
            , c(FALSE, FALSE, TRUE, TRUE, FALSE, NA))





