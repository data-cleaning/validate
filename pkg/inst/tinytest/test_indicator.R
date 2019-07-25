

## indicators ----
i <- indicator(height/weight, mean(height))
cf <- confront(women, i)
expect_equal(length(cf),2)

i <- indicator(as.character(height))
voptions(i, raise="all")
expect_warning(confront(women,i))

expect_equal(length(indicator(mean(x)) + indicator(mean(x)/sd(x))),2)
ii <- indicator(mean(x)) + indicator(mean(y))
expect_true(!any(duplicated(names(ii))))

