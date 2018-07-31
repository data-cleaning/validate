#' @include confrontation.R
NULL

setGeneric("barplot")


#' Plot number of violations
#' 
#' @param height an R object defining height of bars (here, a \code{validation} object)
#' @param ... parameters to be passed to \code{\link[graphics]{barplot}} but not 
#'  \code{height}, \code{horiz}, \code{border},\code{las}, and \code{las}.
#' @param add_legend Display legend?
#' @param add_exprs Display rules?
#' @param colors Bar colors for validations yielding NA or a violation
#' @param topn If specified, plot only the top n most violated calls
#' @param order_by (single \code{character}) order bars decreasingly from top to bottom by the 
#'      number of fails, passes or \code{NA}'s.
#' @param stack_by (3-vector of \code{characters}) Stacking order for bar chart (left to right)
#'
#' @section Credits:
#' The default colors were generated with the \code{RColorBrewer} package of Erich Neuwirth.
#' 
#' @return A list, containing the bar locations as in \code{\link[graphics]{barplot}}
#' 
#' @aliases barplot,validation-method 
#' @example ../examples/barplot.R
#' @export 
#' @family validation-methods
setMethod('barplot',signature('validation'), 
  function(height, ..., order_by = c("fails","passes","nNA")
           , stack_by = c("fails","passes","nNA")
           , topn=Inf, add_legend=TRUE, add_exprs=TRUE
           , colors=c(fails = "#FB9A99",passes = "#B2DF8A", nNA = "#FDBF6F")
           ){
 
    add_legend <- isTRUE(add_legend)
    add_exprs   <- isTRUE(add_exprs)


    order_by <- match.arg(order_by)
    stopifnot(topn>0,is.character(order_by),is.logical(add_legend),is.logical(add_exprs))
    
    # get calls & values from confrontation object
    calls <- sapply(height$._calls, deparse)
    names(calls) <- names(height$._value)
    val <- values(height,drop=FALSE)
    
    # reorder colors to match stacking order
    colors <- colors[stack_by]

    # defaults for some optional parameters
    args <- list(...)
    argn <- names(args)
    xlab <- args$xlab
    if (is.null(xlab)) xlab <- "Items"
    args <- args[argn != "xlab"]

    if ( !'main' %in% argn ) args$main <- deparse(height$._call)
    
    # values with different dimensionality are plotted in different row.
    # we turn xpd off so the legend can be placed outside
    # narrow the margins for more efficient use of plotting region.
    oldpar <- par(mar=c(4,4.1,3,1),xpd=TRUE, mfrow=c(length(val),1))
    on.exit(par(oldpar))
    
    # create plots, one row for each dimension structure
    out <- lapply(seq_along(val), function(i){
      y <- val[[i]]
      count <- cbind(
       nNA = colSums(is.na(y))
       , fails = colSums(!y,na.rm=TRUE)
       , passes = colSums(y,na.rm=TRUE)
      )
      labels <- calls[colnames(y)]
       
      # how to order
      I <- order(count[,order_by])
      count <- count[I,,drop=FALSE]
      labels <- labels[I]
    
      if ( topn < Inf ){
        I <- order(count[,order_by],decreasing=TRUE)
        I <- 1:nrow(count) %in% I[1:min(topn,length(I))]
        count <- count[I,,drop=FALSE]
        labels <- calls[[i]][I]
      }

      if ( !'names.arg' %in% argn ) args$names.arg <- abbreviate(rownames(count))
      arglist <- list(
        height = t(count[,stack_by,drop=FALSE])
        , horiz=TRUE
        , border=NA
        , las=1
        , col=c(colors)
        , xlab=""
      )

      # actual plot
      p = do.call(barplot,c(arglist,args))[seq_along(labels)]
      
      # Add labels & legend
      if (add_exprs) text(0.1,p,labels,pos=4)

      n <- length(labels)
      bw <- if ( n > 1) (p[n]-p[n-1] - 0.2) else 0.2
      ht <- if ( n > 1) p[n]+0.5*bw else 1.197
      m <-  sum(arglist$height[,1])

      text(m-strwidth(xlab)/2, 0.2-0.16*ht,xlab)
      if(add_legend){ 
        legend(x=-0.04*m, y=0.2-0.12*ht
          , legend = stack_by
          , fill=colors
          , border='black'
          , bty='n'
          , horiz=TRUE
       )
      }
      p
    })
    par(xpd=FALSE)
    invisible(out)
})


