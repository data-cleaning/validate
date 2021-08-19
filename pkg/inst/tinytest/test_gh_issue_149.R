df <- data.frame(x = integer(0L))
vl <- validator(x > 0L)
cf <- confront(df, vl)
expect_silent(aggregate(cf))
