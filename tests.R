
fn <- c('parse.R','verifier.R','indicator.R','validator.R','confrontation.R','factory.R','validate.R')
dmp <- lapply(file.path('pkg/R',fn),source)
.onLoad()


v <- validator(height>65,if(height > 66) weight > 140)
cf <- confront(v,women)

w <- validator(height>65,if(height > 66) weight > 140,mean(height)<120,fl==1)
cf <- confront(w,women)

L <- list(w1=women, w2=women*2)
w <- validator(w1$height < w2$height,w2$height>130)
cf <- confront(w,L)





x <- validator(2*x + 3*y < z, x + y > 2)

linear_coefficients(x)




