


library(roxygen2)
roxygenize(package.dir="pkg/",roxygen.dir="pkg/",copy.package=FALSE,unlink.target=TRUE)

fn <- c('verifier.R','indicator.R','validator.R',
        'confrontation.R','parse.R','factory.R'
        ,'validate.R','sugar.R')
dmp <- lapply(file.path('pkg/R',fn),source)


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

w <- validator(
  g : {height; weight}
  , g > 0
  , height/weight > 7
  )




cf$value
cf$calls

# simple analyses

w <- validator(
  x + y > z
  ,x > 0
  ,y > 0
  ,z >= 0
  )

L <- lapply(w$calls,var_from_call)
gr <- stack(L)
names(gr) <- c("source","target")
for ( n in names(gr)) gr[,n] <- as.character(gr[,n])


N <- c(variables(w),names(w))
translate <- seq_along(N)-1
names(translate) <- N

edges <- data.frame(
  source = translate[gr$source]
  ,target = translate[gr$target]
  , value=2
)

vlabel <- sapply(calls(w),call2text)
vlabel[variables(w)] <- variables(w)

nodes = data.frame(
  name= vlabel[names(translate)]
  , group = c(1,1,1,2,2,2,2)
)



library(d3Network)
d3SimpleNetwork(gr,width=500,height=500,file='graph.html')
browseURL("graph.html")

d3ForceNetwork(Links=edges , Nodes=nodes, Source="source", Target="target",Value="value",NodeID="name",Group="group"
               ,width=500,height=500
               , opacity=0.9,zoom=TRUE
,file="graph2.html")
browseURL("graph2.html")
