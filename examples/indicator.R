# create an indicator for the number of missing x in data set


I <- indicator( 
 sum(is.na(.))               # number of missing variables
 , sum(is.na(.[c("x","y")])) # number of missing x and y
 , mean(is.na(.))            # fraction of missing variables
 , sum(x)
 , mean(x)
) 

dat <- data.frame(x=1:2, y=c(NA,1))
C <- confront(dat, I)
values(C)
