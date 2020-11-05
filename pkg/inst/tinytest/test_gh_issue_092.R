
# all() on the result of checking a 0-row data frame must be TRUE
# (every statement about elements of the empty set is TRUE)

# similarly, any() should give FALSE: there are not any elements
# in the empty set for which a statement is TRUE.

cf <- check_that(data.frame(a = integer(0)), a == 1)

expect_true(all(cf))
expect_false(any(cf))



