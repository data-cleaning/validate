rules_df <- data.frame( rule = c("height > 0", "weight > 0", "height < 1.5 * weight")
                      , name = c(1,1,2)
)

rules <- validator(.data = rules_df)
nms <- names(rules)
expect_equal(nms, unique(nms))

