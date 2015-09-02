

# simple use
library(validate)
cf <- check_that(women
      , height>0
      , height/weight < 0.5
      )
summary(cf)

library(magrittr)
women %>% 
  check_that(height > 0, height/weight < 0.5) %>% 
  summary()

# storing rules
data(retailers)

# demo
v <- validator(
  staff >= 0
  , if (staff > 0 ) staff.costs > 0
  , staff.costs < total.costs
  , turnover + other.rev == total.rev
  , profit + total.costs == total.rev
)
cf <- confront(retailers,v)
summary(cf)
barplot(cf,main="Retailers")



# dimension structure
cf <- check_that(women
  , height > 0               # per record
  , cov(height,weight) > 0.9 # involves whole data set
)
summary(cf)


# cross-dataset
v <- validator(height < women2$height)
cf <- confront(women, v, ref=list(women2 = 2*women))
summary(cf)



# fdep
v <- validator(city + street ~ zipcode)
d <- data.frame(
  street = rep("Spui",4)
  , city   = c("The Hague", "The Hague","Amsterdam", "The Hague")
  , zipcode= c(2511,2513,2511,2511)
)
cbind(d, fd_value=values(confront(d,v))[,1])



# groups
v <- validator(
  G := var_group(height, weight)
  , G >=0
)
v
summary(confront(women,v))

# macros H-B outlier check for skewly distributed data

v <- validator(
 r := height/weight
, m := median(r)
, pmax(r/m,m/r)-1 < 0.05
)

summary(confront(women,v))









