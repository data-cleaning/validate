

fn <- c('verifier.R','indicator.R','validator.R',
        'confrontation.R','parse.R','factory.R'
        ,'sugar.R','functions.R')
dmp <- lapply(file.path('pkg/R',fn),source)


x <- indicator(
  mean(height)
  ,height/weight
  , ifelse(height < 65,'short','tall')
  , ifelse ( mean(height) > 70,'b','a')
)

cf <- confront(x,women)
summary(cf)

I <- indicator(number_missing())


V <- validator(height<weight)
w1 <- w2 <- women
w1[1,1] <- NA
w1[2,c(1,2)] <- w1[2,c(2,1)]

compare(V,ruw=w1, gaaf=w2)

V <- validator(
  weight < 159
  , height/weight > 0.48
  ,height > 0
  )
cf <- confront(V,w1)
plot(cf,order=FALSE)

V <- validator(
  height > 0
  , weight > 0
  )
plot(confront(V,women))


vx <- values(cf)
n <- lapply(vx,colSums)
g <- sapply(calls(cf),as.character)
p <- barplot(n[[1]],horiz=TRUE,las=1)
text(rep(0.1,3),p[,1],g[[1]],pos=4)


p <- barplot(1:3,horiz=TRUE,border=NA,ylim=c(0,3),xlim=c(0,3))

abline(h=p,col='red')

w <- 0.4
drawbarat <- function(y,height,width){
  w <- width/2
  polygon(
      c(0,height,height,0)
    , c(y-w/2,y-w/2,y+w/2,y+w/2)
    ,col='black'
    ,border=NA
  )
}

barplot(3:1,horiz=TRUE,width=0.4,space=1.,add=TRUE,col='black',ylim=c(0,3),xlim=c(0,3))


cf1 <- confront(V,w1)
cf2 <- confront(V,w2)

compare2(cf1,cf2)

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
