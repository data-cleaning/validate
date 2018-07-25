rules <- validator( r1 = staff.costs < total.costs
                  , r2 = turnover + other.rev == total.rev
                  , r3 = other.rev > 0
                  , r4 = total.rev > 0
                  )
plot(rules, cex=0.8)

data(retailers)
cf <- confront(retailers, rules)
plot(cf, main="Retailers check")
