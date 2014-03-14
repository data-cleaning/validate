
fn <- c('verifier.R','indicator.R','validator.R',
        'confrontation.R','parse.R','factory.R'
        ,'validate.R','sugar.R')
dmp <- lapply(file.path('pkg/R',fn),source)
.onLoad()


w <- validator(
  g : {height; weight}
  , w1$g > w2$g
  , mean(w1$g) > mean(w2$g)
  , x > 0
  , w1[,'height'] > 65
  , w1[, 'hite'] > 120
)

women[1,2] <- NA

cf <- confront(w,list(w1=women, w2=women/2))
summary(cf)

cf$value
cf$calls

# simple analyses

