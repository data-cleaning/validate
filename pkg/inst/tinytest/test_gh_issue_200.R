df <- data.frame(a = 1:4)
rules <- validator(is.numeric(a), b %in% LETTERS)

expect_false(all(confront(df, rules), fail_on_error = TRUE))
expect_true(any(confront(df, rules), fail_on_error = TRUE))
expect_false(any(confront(df, rules[2]), fail_on_error = TRUE))

