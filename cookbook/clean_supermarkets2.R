## Contents of clean_supermarkets2.R
library(validate)

#1.a simulate reading data
data(SBS2000, package="validate")
spm <- SBS2000[c("id","staff","other.rev","turnover","total.rev")]

# 1.b Create rule set
rules <- validator(staff >= 0, other.rev>=0, turnover>=0
                 , other.rev + turnover == total.rev)


# 2. add two loggers 
start_log(spm, logger=lbj_cells())
start_log(spm, logger=lbj_rules(rules))

# 3. assume empty values should be filled with 0
spm <- transform(spm, other.rev = ifelse(is.na(other.rev),0,other.rev))

# 4. assume that negative amounts have only a sign error
spm <- transform(spm, other.rev = abs(other.rev))

# 5a. ratio estimator for staff conditional on turnover
Rhat <- with(spm, mean(staff,na.rm=TRUE)/mean(turnover,na.rm=TRUE))

# 5b. impute 'staff' variable where possible using ratio estimator
spm <- transform(spm, staff = ifelse(is.na(staff), Rhat * turnover, staff))

# 6. write output
write.csv(spm, "supermarkets_treated.csv", row.names = FALSE)


