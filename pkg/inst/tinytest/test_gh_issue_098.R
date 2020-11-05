
v <- validator(
  other.rev >= 0
  , total.rev >= 0
  , staff.costs >= 0
  , total.costs >= 0
  , turnover + other.rev == total.rev
  , total.rev - total.costs == profit
)

blocks <- .blocks_expressionset(v)

expect_equal(length(blocks), 2)
expect_equal( length(intersect(blocks[[1]], blocks[[2]] )), 0  )


