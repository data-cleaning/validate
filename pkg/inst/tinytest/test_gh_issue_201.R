d <- data.frame(a = c(1, 1), b  = c(1, 2))
val <- validator(a ~ b)
out <- confront(d, val)
expect_equal(nrow(violating(d, out)), 2)


