
fn <- c('verifier.R','indicator.R','validator.R','confrontation.R','factory.R','validate.R')
dmp <- lapply(file.path('pkg/R',fn),source)

v <- validator(height>65)
confront(v,women)

L <- list(w1=women, w2=women*2)
w <- validator(w1$height < w2$height,w2$height>130)
cf <- confront(w,L)



