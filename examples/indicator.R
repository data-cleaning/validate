# create an indicator for the number of missing x in data set
I <- indicator( 
   number_missing(x)     # number of missing x
 , number_missing(x,y)   # number of missing x and y
 , number_missing()      # number of missing variables
 , fraction_missing()    # fraction of missing variables
 , sum(x)
 , mean(x)
) 

dat <- data.frame(x=1:2, y=c(NA,1))
C <- confront(I, dat)
values(C)
