data(retailers)
cf <- check_that(retailers
    , staff.costs < total.costs
    , turnover + other.rev == total.rev
    , other.rev > 0
    , total.rev > 0)
barplot(cf)

