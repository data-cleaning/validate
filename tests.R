

fn <- c('expressionset.R','indicator.R','validator.R',
        'confrontation.R','compare.R','parse.R','factory.R'
        ,'sugar.R','functions.R')
dmp <- lapply(file.path('pkg/R',fn[1]),source)



rm(list=ls(all.names = TRUE))



