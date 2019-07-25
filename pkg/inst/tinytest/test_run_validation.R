
expect_silent(out <- run_validation_file("run_validation/validations.R", verbose=FALSE))

expect_equal(length(out), 2)
expect_equal(length(out[[1]]),3)
expect_equal(length(out[[2]]),1)


# Methods
s <- summary(out)
s1 <- summary(out[[1]])


# the summary of a 'validations' object has 4 extra columns:
# file, call, first line nr, last line nr.
expect_equal(ncol(s), ncol(s1) + 4)


