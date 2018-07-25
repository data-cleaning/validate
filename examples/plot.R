rules <- validator( cost = staff.costs < total.costs
                  , bal  = turnover + other.rev == total.rev
                  , revO = other.rev > 0
                  , revT = total.rev > 0
                  )
plot(rules, cex=0.8)

data(retailers)
cf <- confront(retailers, rules)
plot(cf, main="test")
