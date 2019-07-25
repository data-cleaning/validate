

## issue #91 is solved ----
data <- data.frame(A = 1)
rule <- validator(A > 0)
cf <- confront(data, rule)
expect_silent(plot(rule))
expect_silent(plot(cf))


