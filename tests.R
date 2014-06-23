

fn <- c('verifier.R','indicator.R','validator.R',
        'confrontation.R','compare.R','parse.R','factory.R'
        ,'sugar.R','functions.R')
dmp <- lapply(file.path('pkg/R',fn),source)




v <- validator(
  fiets := 2*height
  , fiets > 126
)
cf <- confront(v,women)
summary(cf)

e <- new.env()







cls <- calls(v)
w <- new.env()
lapply(cls[is.assignment(calls)],eval,envir=w)
ls(w)
lapply(cls[!is.assignment(calls)],eval,envir=women,enclos=w)


v <- validator(height>0)
confront(v,women)

data <- women
data$aap <- data$height/data$weight
data$aap[1] <- NA
v <- validator(number_missing("a.p") == 0)

confront(v,data)
#

L <- lapply(dir("~/projects/tmp/nuttig/stappen/",full.names=TRUE),read.csv2)
L <- match_data(.list=L)

cl <- cells(.list=(L))
plot(cl)



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
