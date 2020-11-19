#' @include confrontation.R
NULL

# helper to make possiby large numbers more readable.
# x: nonnegative integer.
humanize <- function(x){
  out <- character(length(x))
  i <- x < 1000
  out[i] <- sprintf("%3.0f",x[i])
  i <- x>= 1000 & x < 1e5 
  out[i] <- sprintf("%2.1fk", x[i]/1000)
  i <- x >= 1e5  &  x < 1e6 
  out[i] <- sprintf("%3.0fk", x[i]/1000)
  i <- x >= 1e6  &  x < 1e8 
  out[i] <- sprintf("%2.1fM", x[i]/1e6)
  i <- x > 1e8 & x < 1e9
  out[i] <- sprintf("%3.0fM", x[i]/1e6)
  i <- x >= 1e9  &  x < 1e11 
  out[i] <- sprintf("%2.1fMM", x[i]/1e9)
  i <- x > 1e11 & x < 1e12
  out[i] <- sprintf("%3.0fMM", x[i]/1e9)
  i <- x >= 1e12
  out[i] <- sprintf("%g",x[i])
  trimws(out)
}



# Add bar plot to current viewport.
# depending on the number of bars, white lines are drawn between them
# and the axes are annotated.
# m: a matrix with columns representing [fails, passes, NA]
fill_bars <- function(m, fill, col=fill, rulenames){
  m <- prop.table(m,1)
  
  n <- nrow(m)
  h <- 1/n
  y <- seq(0,n-1)*h
  ii <- order(m[,1])
  m <- m[ii,,drop=FALSE]
  rulenames <- rulenames[ii]

  grid::grid.rect(x=0, y= y, height=h, width=m[,1]
            , just=c("left","bottom")
            , gp=gpar(fill=fill[1],col=col[1]))
  grid::grid.rect(x=m[,1],y=y, height=h, width=m[,2]
            , just=c("left","bottom")
            , gp=gpar(fill=fill[2], col=col[2]))
  grid::grid.rect(x=1-m[,3],y=y, height=h, width=m[,3]
            , just=c("left","bottom")
            , gp=gpar(fill=fill[3],col=col[3]))
  grid::grid.xaxis(at=c(0:5)/5, label=paste0(seq(0,100,by=20),"%"))
  
  if (nrow(m)<50){ 
    grid::grid.text(rulenames,x=unit(-0.3,"lines"), y=unit(y+h/2,"npc")
                  , just=c("right","center"))
    for ( i in seq(2,length(y))){ 
      grid::grid.abline(intercept=y[i], slope=0, gp=grid::gpar(col="white",lwd=0.5))
    }
  }
}



# Add legend to current viewport.
# m: a matrix with columns representing [fails, passes, NA]
add_vlegend <- function(m, width, labels, fill, color=fill){

  ns <- humanize(colSums(m))
  ns <- c(ns, humanize(sum(m)))
  
  perc <- colSums(m)/sum(m)*100
  perc <- c(perc,100)
  
  for ( j in 1:4){
    grid::pushViewport(grid::viewport(layout.pos.row=1, layout.pos.col=j))
    # first row: boxes + labels
    grid::grid.rect(x=grid::unit(0,"char")
              , y=grid::unit(1.5,"lines") - grid::unit(1,"char")
              , width=grid::unit(1,"char"), height=grid::unit(1,"char")
              , just = c("left","bottom")
              , gp = grid::gpar(fill=fill[j], col = color[j]))
    grid::grid.text(labels[j]
                  , x = grid::unit(1.2,"char")
                  , y = grid::unit(0.9,"char")
                  , just=c("left","bottom"))
    grid::upViewport()
    # second row: numbers of items
    grid::pushViewport(grid::viewport(layout.pos.row=2, layout.pos.col=j))
    grid::grid.text(sprintf("%s",ns[j]),x=unit(1.2,"char"), just=c("left","bottom"))
    grid::upViewport()
    # third row: percentages
    grid::pushViewport(grid::viewport(layout.pos.row=3, layout.pos.col=j))
    grid.text(trimws(sprintf("%3.0f%%",perc[j]))
            , x = grid::unit(1.2,"char"), just=c("left","bottom"))
    upViewport()
  }
}




# plot a validation object
# m: a matrix with columns representing [fails, passes, NA]
plot_validation <- function(m
                    , fill
                    , col=fill
                    , rulenames 
                    , labels
                    , title 
                    , xlab)
{
  grid.newpage()
  # emperically, we can put about 2 characters in one line height.
  left_margin = max(nchar(rulenames))/2
  if (length(rulenames)>50) left_margin <- 1
  main   <- grid::plotViewport(c(5,left_margin,1,1), gp=gpar(fontsize=8))
  grid::pushViewport(main)
  fill_bars(m, fill=fill, col=col,rulenames=rulenames)
  ii <- which.max(nchar(labels))
  box_width <- grid::stringWidth(labels[ii])
  grid::upViewport()

  # add title  
  if (is.null(title)) title <- "Validation results by rule"
  vptitle <- grid::viewport(x = main$x
                          , y      = main$y + main$height
                          , width  = grid::stringWidth(title)
                          , height = grid::unit(1,"lines")
                          , just   = c("left","bottom"))
  pushViewport(vptitle)
  grid::grid.text(title
                , x    = grid::unit(0,"npc")
                , y    = grid::unit(0.2,"char")
                , just = c("left","bottom"))
  grid::upViewport()
  
  # add xlabel
  if (is.null(xlab)) xlab <- "Items"
  vpxlab <- grid::viewport(x = main$x
                         , y      = main$y -grid::unit(2,"lines")
                         , width  = grid::stringWidth(xlab)
                         , height = grid::unit(1,"lines")
                         , just   = c("left","top"))
  grid::pushViewport(vpxlab)
  grid::grid.text(xlab
                , x    = grid::unit(0,"npc")
                , y    = grid::unit(0,"npc")
                , just = c("left","bottom"))
  grid::upViewport()
  
  # add legend
  ncols <- length(labels)
  lgd <- grid::viewport(x = main$x+main$width
                 , y      = grid::unit(0,"char")
                 , width  = ncols*box_width
                 , height = grid::unit(3,"lines")
                 , just   = c("right","bottom")
                 , gp     = grid::gpar(fontsize=8)
                 , layout = grid::grid.layout(3, ncols
                          , widths = rep(grid::unit(1/ncols,"null"), ncols)
                          ,  heights=rep(unit(1/3,"null"),ncols)) 
                  )
  pushViewport(lgd)
  add_vlegend(m, width=box_width, labels=labels, fill=fill, color=col)
}














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

    warning("The 'barplot' method for confrontation objects is deprecated. Use 'plot' instead"
            ,call.=FALSE)
 
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



