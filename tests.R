
fn <- c('verifier.R','indicator.R','validator.R',
        'confrontation.R','parse.R','factory.R'
        ,'validate.R','sugar.R')
dmp <- lapply(file.path('pkg/R',fn),source)
.onLoad()


w <- validator(
  g : {height; weight}
  , w1$g > w2$g
  , mean(w1$g) > mean(w2$g)
)

do.call(validator,calls(w))


cf <- confront(w,list(w1=women, w2=women/2))

cf$value
cf$calls

# simple analyses
data.frame(
  validator = names(cf$value)
  , confrontations = sapply(cf$value,length)
  , passes = sapply(cf$value,sum,na.rm=TRUE)
  , call = sapply(cf$calls,call2text)
  )

