## issue #194 is solved ----
data <- data.frame(FRUIT = c("apple", "banana", "pear"))

expect_silent(starts_rule <- validator(startsWith(FRUIT, "a")))
expect_silent(ends_rule <- validator(endsWith(FRUIT, "a")))

cf_starts <- as.data.frame(confront(data, starts_rule))
cf_ends <- as.data.frame(confront(data, ends_rule))

expect_equal(cf_starts$value, c(TRUE, FALSE, FALSE))
expect_equal(cf_ends$value, c(FALSE, TRUE, FALSE))
