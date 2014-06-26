

fn <- c('expressionset.R','indicator.R','validator.R',
        'confrontation.R','compare.R','parse.R','factory.R'
        ,'sugar.R','functions.R')
dmp <-lapply(file.path('pkg/R',fn),source)



v <- validator(a := height,height > 0)
cf <- confront(v,women)
summary(cf)

v <- validator(
  G : {height; weight}
  , G > 0
  )
v
summary(confront(v,women))


rm(list=ls(all.names = TRUE))



