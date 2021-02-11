
expect_silent(
  add_indicators(women
      , confront(women, indicator(mn=mean(height) )))
)

ii <- indicator(ratio=height/weight, mnw =mean(weight), mht = mean(height))
out <- confront(women, ii)
d <- add_indicators(women, out)
expect_equal(d$mnw, rep(mean(women$weight), nrow(women)) )
expect_equal(d$mht, rep(mean(women$height), nrow(women)) )



