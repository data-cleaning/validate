
## TODO: decide whether we move all visualisations & reporting to a different package.

# Create a visualisation
# 
# @param x an R object
# @param ... parameters to be passed to \code{\link[graphics]{barplot}} but not 
#  \code{height}, \code{horiz}, \code{border},\code{las}, \code{xlim}, and \code{las}.
# @param add_legend Display legend?
# @param add_calls Display rules?
# @param colors Bar colors for validations yielding NA or a violation
# @param topn If specified, plot only the top n most violated calls
# @param order order the bars from most to least violated rules
# 
# 
# @return A list, containing the bar locations as in \code{\link[graphics]{barplot}}
#
# 
setMethod('plot',signature('validatorValue'), 
 function(x, ..., order=TRUE, topn=Inf, add_legend=TRUE, add_calls=TRUE
          , colors=c("#A6CEE3CC", "#1F78B4CC") ){
   
   stopifnot(topn>0,is.logical(order),is.logical(add_legend),is.logical(add_calls))
   calls <- lapply(calls(x),sapply,call2text)
   val <- values(x)
   
   # values with different dimensionality are plotted in different row.
   par(mfrow=c(length(calls),1),xpd=TRUE)
   
   out <- lapply(seq_along(val), function(i){
     y <- val[[i]]
     count <- cbind(
       nna = colSums(is.na(y)),
       nvio = colSums(!y,na.rm=TRUE)
     )
     labels <- calls[[i]]
     
     # how to order
     if (order) count <- count[order(count[,'nvio']),,drop=FALSE]    
     if ( topn < Inf ){
       I <- order(count[,'nvio'],decreasing=TRUE)
       I <- 1:nrow(count) %in% I[1:min(topn,length(I))]
       count <- count[I,,drop=FALSE]
       labels <- calls[[i]][I]
     }
     # plot
     xlim = c(0, max(count))
     p = barplot(count[,'nna'],horiz=TRUE,border=NA,xlim=xlim,las=1,col=colors[1],...)
     for ( j in seq_along(p) ) drawbarat(p[j],count[j,'nvio'],0.4,col=colors[2])
     
     # labels & legend
     if (add_calls) text(0.1,p,labels,pos=4)
     if(add_legend){ 
       legend('topright'
              , legend = c('Missing','Violated')
              , fill=colors
              , border=colors
              , bty='n'
              , horiz=TRUE
              , inset=c(0,-0.1)
       )
     }
     p
   })
   par(xpd=FALSE)
   invisible(out)
 }
)

drawbarat <- function(y,height,width,col){
 w <- width/2
 polygon(
   c(0,height,height,0)
   , c(y-w,y-w,y+w,y+w)
   ,col=col
   ,border=NA
 )
}

