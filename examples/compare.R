data(retailers)

rules <- validator(turnover >=0 0, staff>=0,other.rev>=0)

# start with raw data
step0 <- retailers

# impute turnovers
step1 <- step0
step1$turnover[is.na(step1$turnover)] <- mean(step1$turnover,na.rm=TRUE)

# flip sign of negative revenues
step2 <- step1
step2$other.rev <- abs(step2$other.rev)
  
# create an overview of differences compared to raw data
compare(raw = step0, imputed = step1, flipped = step2)

# create an overview of differences, comparing to the previous step
out <- compare(raw = step0, imputed = step1, flipped = step2, compare="sequential")
out

# transform data to data.frame (easy for use with ggplot)
as.data.frame(out)

plot(out)

