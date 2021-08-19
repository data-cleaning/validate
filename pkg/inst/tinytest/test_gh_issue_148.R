
df <- data.frame(rule = c("x>0","x<1"), name=c(NA,"bla"))
expect_equal(names(validator(.data=df)),c("V1","bla"))


