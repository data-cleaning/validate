# Reported by Matthias Gomolka

# Subsetting confrontation objects does not work within lapply

df <- data.frame(a = 1:5, b = 3)
vl <- validator(a_gt_b = a > b, a_eq_b = a == b)
cf <- confront(df, vl)
names <- names(vl)

expect_silent(lapply(names, function(i) cf[i]))


