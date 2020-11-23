

## indicators ----
i <- indicator(height/weight, mean(height))
cf <- confront(women, i)
expect_equal(length(cf),2)

i <- indicator(as.character(height))
voptions(i, raise="all")
expect_warning(confront(women,i))

expect_equal(length(indicator(mean(x)) + indicator(mean(x)/sd(x))),2)
ii <- indicator(mean(x)) + indicator(mean(y))
expect_true(!any(duplicated(names(ii))))


# add indicators directly to data frame
ii <- indicator(
    hihi = 2*sqrt(height)
  , haha = log10(weight)
  , lulz = mean(height)
  , wo0t = median(weight)
)
out <- confront(women, ii)
expect_equal(ncol(add_indicators(women, ii)),  ncol(women) + length(ii))
expect_equal(ncol(add_indicators(women, out)), ncol(women) + length(ii))


ii <- indicator(
BMI = (weight/2.2046)/(height*0.0254)^2
, mh = mean(height)
, mw = mean(weight))

out <- confront(women, ii)

expect_equal(names(add_indicators(women, out))
           , c(names(women), names(ii)) ) 




