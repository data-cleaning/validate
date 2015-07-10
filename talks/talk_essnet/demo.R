

library(editrules)
retailers <- read.csv2("pkg/data/retailers.csv")
edits <- editfile("myedits.txt")

ve <- violatedEdits(edits,retailers)
summary(ve)
plot(ve)
plot(edits)

el <- localizeErrors(edits, retailers, method="mip")
summary(el)
plot(el)

isFeasible(edits)
isObviouslyRedundant(edits)

#

library(validate)
v <- validator(.files="myedits2.txt")
retailers <- read.csv2("retailers.csv")
cf <- confront(v, retailers)









v <- validator(.files="myedits2.txt")
cf <- confront(v,retailers)




vd <- validator(city + street ~ zipcode)

dat <- data.frame(
  street = c('kerkstraat','kerkstraat','kerkstraat','kerkstraat')
  , city = c('DH','DH','H','DH')
  , zipcode = c('2495','2496','8888','2495')
)

cf <- confront(vd,dat)
values(cf)



