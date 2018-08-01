#' @include validator.R
#' @include indicator.R 
#' @include confrontation.R
NULL

setClassUnion('callOrNull',list('call','NULL'))


### COMPARE ----


# Mark an array as comparison so we can overload the 'show' and plot' function.
setClass('comparison', contains='array' 
  , slots=list(call = 'callOrNull')
  , prototype = prototype(array(0,dim=c(0,0)), call=NULL)
)

setClass('validatorComparison',contains='comparison')

setMethod('show',signature('comparison'),function(object){
  cat(sprintf('Object of class %s:\n',class(object)))
  cat(sprintf('\n   %s\n\n',call2text(object@call)))
  print(object[,])
})


#' Compare similar data sets
#'
#' Compare versions of a data set by comparing their performance against a
#' set of rules or other quality indicators. This function takes two or
#' more data sets and compares the perfomance of data set \eqn{2,3,\ldots}
#' against that of the first data set (default) or to the previous one
#' (by setting \code{how='sequential'}).
#'
#' @param x An R object
#' @param ... data frames, comma separated. Names become column names in
#'   the output. 
#' 
#' @example ../examples/compare.R
#' @export
setGeneric('compare', def = function(x,...) standardGeneric('compare'))



#' @section Comparing datasets by performance against validator objects:
#'
#' Suppose we have a current and a previous version of a data set. Both
#' can be inspected by \code{\link{confront}}ing them with a rule set.
#' The status changes in rule violations can be partitioned as shown in the 
#' following figure.
#' \if{html}{\figure{rulesplit.png}{options: width=80\% alt="cellwise splitting"}}
#' \if{latex}{\figure{rulesplit.pdf}{options: width=13cm}}
#' This function computes the partition for two or more
#' datasets, comparing the current set to the first (default) or to the 
#' previous (by setting \code{compare='sequential'}).
#'
#' @references
#' The figure is reproduced from MPJ van der Loo and E. De Jonge (2018)
#' \emph{Statistical Data Cleaning with applications in R} (John Wiley & Sons).
#'
#' @param how how to compare
#' @param .list Optional list of data sets, will be concatenated with \code{...}.
#' @rdname compare
#' 
#'
#' @return 
#' For \code{validator}: An array where each column represents 
#' one dataset. 
#' The rows count the following attributes:
#' \itemize{
#' \item{Number of validations performed}
#' \item{Number of validations that evaluate to \code{NA} (unverifiable)}
#' \item{Number of validations that evaluate to a logical (verifiable)}
#' \item{Number of validations that evaluate to \code{TRUE}}
#' \item{Number of validations that evaluate to \code{FALSE}}
#' \item{Number of extra validations that evaluate to \code{NA} (new unverifiable)}
#' \item{Number of validations that still evaluate to \code{NA} (still unverifialble)}
#' \item{Number of validations that still evaluate to \code{TRUE}}
#' \item{Number of extra validations that evaluate to \code{TRUE} }
#' \item{Number of validations that still evaluate to \code{FALSE}}
#' \item{Number of extra validations that evaluate to \code{FALSE}}
#' }
#' 
#' @family validation-methods
#' @family comparing
#' @export 
setMethod("compare", "validator",
  function(x,... , .list=list(), how=c("to_first","sequential")){
    L <- c(list(...),.list)  
    names(L) <- make_listnames(L)
    how <- match.arg(how)
    
    out <- if (how == "to_first"){
      cbind(
        rules_diff(x, L[[1]])
      , vapply( seq_len( length(L) - 1 )
          , function(i) rules_diff(x, L[[i+1]], L[[1]])
          , FUN.VALUE = numeric(11) )
      )

    } else {
      cbind(
        rules_diff(x, L[[1]])
      , vapply( seq_len( length(L) - 1 )
          , function(i) rules_diff(x, L[[i+1]], L[[i]])
          , FUN.VALUE = numeric(11) )
      )
    }
    colnames(out) <- names(L)
    names(dimnames(out)) <- c("Status","Version")
    new('validatorComparison'
      , out 
      , call = match.call(definition=compare, sys.call(sys.parent(1L)))
    )
})



rules_diff <- function(rules, new, old=NULL){
  cf_new <- values(confront(new,rules),simplify=FALSE)

  validations  = sum( sapply(cf_new, length) )
  verifiable   = sum( sapply(cf_new, function(x) sum(!is.na(x))) )
  unverifiable = sum( sapply(cf_new, function(x) sum( is.na(x))) )
  violated     = sum(sapply(cf_new, function(x) sum(!x, na.rm=TRUE)))
  satisfied    = sum( sapply(cf_new, sum, na.rm=TRUE) )

  if ( is.null(old) ){
    still_unverifiable = unverifiable
    new_unverifiable   = 0
    still_satisfied    = satisfied
    new_satisfied      = 0
    still_violated     = violated
    new_violated       = 0
  } else {
    cf_old <- values(confront(old,rules),simplify=FALSE)
    still_unverifiable = local({
      s <- 0
      for ( i in seq_len(length(cf_new)) ) {
        s <- s + sum( is.na(cf_new[[i]]) & is.na(cf_old[[i]]) )
      }
      s
     })
    new_unverifiable   = local({
      s <- 0
      for ( i in seq_len(length(cf_new)) ){
        s <- s + sum(!is.na(cf_old[[i]]) & is.na(cf_new[[i]]))
      }
      s
     })
    still_satisfied  = local({
      s <- 0
      for ( i in seq_len(length(cf_new)) ){
        s <- s + sum(cf_old[[i]] & cf_new[[i]], na.rm=TRUE)
      }
      s
    })
    new_satisfied    = local({
      s <- 0
      for ( i in seq_len(length(cf_new)) ){
        s <- s + sum((!cf_old[[i]]|is.na(cf_old[[i]])) & cf_new[[i]], na.rm=TRUE)
      }
      s
    })
    still_violated   = local({
      s <- 0
      for ( i in seq_len(length(cf_new)) ){
        s <- s + sum(!cf_old[[i]] & !cf_new[[i]], na.rm=TRUE)
      }
      s
    })
    new_violated     = local({
      s <- 0
      for ( i in seq_len(length(cf_new)) ){
        s <- s + sum((cf_old[[i]]|is.na(cf_old[[i]])) & !cf_new[[i]], na.rm=TRUE)
      }
      s
    })
  } # end else

  # output
  c(validations         = validations
  , verifiable          = verifiable
  , unverifiable        = unverifiable
  ,  still_unverifiable = still_unverifiable
  ,  new_unverifiable   = new_unverifiable
  , satisfied           = satisfied
  ,  still_satisfied    = still_satisfied
  ,  new_satisfied      = new_satisfied
  , violated            = violated
  ,  still_violated     = still_violated
  ,  new_violated       = new_violated)

}



make_listnames <- function( L, base=sprintf("D%04d",seq_along(L)) ){
  nm <- names(L)
  if (is.null(nm)) return(base)
  nm[nm==""] <- base[nm==""]
  nm
}

#' Translate a validatorComparison object to data frame
#'
#' The performance of versions of a data set with regard to rule-based quality
#' requirements can be compared using using \code{\link{compare}}. The result is a
#' \code{validatorComparison} object, which can usefully be translated into a data
#' frame.
#'
#' @inheritParams as.data.frame
#' 
#' @return A data frame with the following columns.
#' \itemize{
#'  \item{\code{status}: Row names of the \code{validatorComparison} object.}
#'  \item{\code{version}: Column names of the \code{validatorComparison} object.}
#'  \item{\code{count}: Contents of the \code{validatorComparison} object.}
#' }
#' 
#' 
#' @example ../examples/compare.R
#' @family comparing
#' @export
setMethod("as.data.frame","validatorComparison", function(x,...){
  x <- x[,]
  class(x) <- "table"
  setNames(as.data.frame(x,...),c("status","version","count"))
})


#' Line graph of validatorComparison object
#' 
#' The performance of versions of a data set with regard to rule-based quality
#' requirements can be compared using using \code{\link{compare}}. The result is a
#' \code{validatorComparison} object. This method creates a line-graph, thus
#' suggesting an that an ordered sequence of data sets have been compared.  See
#' also \code{\link{barplot,validatorComparison-method}} for an unordered version.
#' 
#'
#' @param x Object of class \code{validatorComparison}.
#' @param xlab [\code{character}] label for x axis (default none)
#' @param ylab [\code{character}] label for y axis (default none)
#' @param las [\code{numeric}] in \code{{0,1,2,3}} determining axis label rotation
#' @param cex.axis [\code{numeric}] Magnification with respect to the current
#'   setting of \code{cex} for axis annotation.
#' @param cex.legend [\code{numeric}] Magnification with respect to the current
#'   setting of \code{cex} for legend annotation and title.
#' @param ... Graphical parameters, passed to \code{plot}. See \code{\link[graphics]{par}}.
#'
#' @family comparing
#' @export
setMethod("plot", "validatorComparison"
 , function(x
    , xlab=""
    , ylab=""
    , las=2
    , cex.axis=0.8
    , cex.legend=0.8,...){

  oldpar <- par(mar=c(3,3,3,8),cex=1)
  on.exit(par(oldpar))

  status <- rownames(x)
  version <- colnames(x)
  dat <- as.data.frame(x)

  # set color palettes, line style and line width mappings
  cl_map <- lw_map <- lt_map <- setNames(vector(11,mode = "integer"), status)
  lt_map[1:11] <- 1 
  lt_map[grepl("new",names(lt_map))] <- 2
  
  lw_map[1:11] <- 1
  lw_map[!grepl("(new)|(still)",names(lw_map))] <- 2
  
  
  # Colors taken from brewer.pal(6,"Paired")
  cl_map[1:11]                                 <- "black"
  cl_map[grepl("unverifiable", names(cl_map))] <- "#A6CEE3"
  cl_map[grepl("satisfied", names(cl_map))]    <- "#33A02C"
  cl_map[grepl("violated", names(cl_map))]     <- "#E31A1C"
  cl_map["verifiable"]                         <- "#1F78B4"
  
  # setup plot
  n <- length(version)
  plot(0,0
   , col      = 'white'
   , xlim    = c(1,length(version))
   , ylim    = c(0,max(x))
   , las     = las
   , xaxt    = 'n'
   , xlab    = xlab
   , ylab    = ylab
   , cex.axis= cex.axis
   , ...)
  # vertical grid
  abline(v = seq_along(version), col = "grey", lty = 3)
  # graph the main lines
  for (stat in status){
    d <- dat[dat$status == stat, ]
    lines( as.integer(d$version), d$count
       , type='b', col=cl_map[stat]
       , lw=lw_map[stat], lt=lt_map[stat], pch=16)  
  }
  axis(side=1, labels=version, at=seq_along(version)
      , las=1, padj=c(0,1)
      , cex.axis=cex.axis)
  # add legend
  oldpar <- c(oldpar,par(xpd=TRUE))
  legend(x=1.04*n,y=max(x)
    , legend = gsub("_"," ",status)
    , lwd = lw_map[status]
    , lty = lt_map[status]
    , col = cl_map[status]
    , cex = cex.legend
    , bty = "n"
    , title = "Count"
  )
  invisible(NULL)
})


#' Barplot of validatorComparison object
#'
#' The performance of versions of a data set with regard to rule-based quality
#' requirements can be compared using using \code{\link{compare}}. The result is a
#' \code{validatorComparison} object. This method creates a stacked bar plot of
#' the results.  See also \code{\link{plot,validatorComparison-method}} for a line
#' chart.
#'
#' @param height  object of class \code{validatorComparison}
#' @param las [\code{numeric}] in \code{{0,1,2,3}} determining axis label rotation
#' @param cex.axis [\code{numeric}] Magnification with respect to the current
#'   setting of \code{cex} for axis annotation.
#' @param cex.legend [\code{numeric}] Magnification with respect to the current
#'   setting of \code{cex} for legend annotation and title.
#' @param wrap [\code{logical}] Toggle wrapping of x-axis labels when their width
#'  exceeds the width of the column.
#' @param ... Graphical parameters passed to \code{\link[graphics]{barplot.default}}.
#'
#' @note Before plotting, underscores (\code{_}) and dots (\code{.}) in x-axis labels
#' are replaced with spaces.
#' 
#' @example ../examples/compare.R
#' @family comparing
#' @export
setMethod("barplot", "validatorComparison", function(height
    , las = 1
    , cex.axis = 0.8
    , cex.legend = cex.axis
    , wrap = TRUE
    , ...){

  oldpar <- par(mar=c(3,3,3,8), xpd=TRUE)
  on.exit(par(oldpar))
  # turn into array
  a <- height[,,drop=FALSE]

  # Colors taken from RColorBrewer::brewer.pal(8,"Paired")
  cl_map <- c(
      "still_satisfied"    = "#33A02C" # dark green
    , "new_satisfied"      = "#B2DF8A" # light green
    , "still_unverifiable" = "#FF7F00" # dark yellow/orange
    , "new_unverifiable"   = "#FDBF6F" # light yellow
    , "still_violated"     = "#E31A1C" # dark red
    , "new_violated"       = "#FB9A99" # light red
  )


  a <- a[names(cl_map),,drop=FALSE]
  x <- barplot(a
      , col=cl_map
      , las=las
      , xaxt="n"
      , cex.axis=cex.axis
      , ...
  )
  xlabs <- colnames(a)
  # simple wrapping heuristic
  if (wrap){
    # replace punctuation with spaces
    xlabs <- trimws(gsub("[_.]"," ", colnames(a)))
    # determine column width
    colw <- if ( length(x) == 1) 1.0 else x[length(x)] - x[length(x)-1] - 0.2
    i <- strwidth(xlabs) > colw
    ncol <- ceiling(colw/strwidth("m"))
    # wrap and fold
    xlabs[i] <- sapply(xlabs[i]
      , function(s) paste(strwrap(s, width=ncol),collapse="\n") 
    )
    
  }
  axis(side=1, labels=xlabs, at=x, cex.axis=cex.axis,lwd=0)

  # compute legend position
  leg_pos <- bp_leg_pos(x)

  legend(x = leg_pos, y = sum(a[,1])
       , legend= rev(sub("_"," ", names(cl_map)))
       , fill=rev(cl_map),bty="n"
       , title="Count", cex=cex.legend)
  invisible(x)  
})

bp_leg_pos <- function(x){
  n <- length(x)
  # the factor 1.04 is the default location of the
  # box after the end of the scale.
  leg_pos <- if (n ==1){
      # 1.2 is default width for n==1
      1.04 * 1.2
  } else {
      # bar width + 0.2 (default) separation
   1.04 * (x[n] + (x[n] - x[n-1]-0.2)/2)
  }

}





setClass('indicatorComparison',contains='comparison')


#' @return For \code{indicator}: A list with the following components:
#' \itemize{
#' \item{\code{numeric}: An array collecting results of scalar indicator (e.g. \code{mean(x)}).}
#' \item{\code{nonnumeric}: An array collecting results of nonnumeric scalar indicators (e.g. names(which.max(table(x))))}
#' \item{\code{array}: A list of arrays, collecting results of vector-indicators (e.g. x/mean(x))}
#' }
#' 
#' @rdname compare
setMethod('compare','indicator',
  function(x, ...,.list=NULL){
    L <- c( list(...), .list)
    if ( length(L) < 2 ) stop('you need at least two datasets')
    names(L) <- make_listnames(L)
    for ( i in seq_along(L) ){
      if ( !matches(L[[1]],L[[i]]) ) 
        stop('dataset ',names(L)[i],'does not match with dataset',names(L)[1])
    }
    n <- names(x)
    v <- setNames( lapply(n, function(i) sapply(L, function(y) values(confront(x[i],y))[[1]] )), n)
    # simplify where possible
    is_array <- sapply(v,is.array)
    is_numeric <- sapply(v,is.numeric)
    w <- if (any(is_array) ){
      lapply(v[is_array],function(x)new('indicatorComparison',x,call=sys.call(2)))
    } else {
      NULL
    }
    u <- if(any(is_numeric & !is_array)){
      new('indicatorComparison',sapply(v[ is_numeric & !is_array],Id),call=sys.call(2))
    } else {
      NULL
    }
    v <- if (any(!is_numeric &!is_array)){
      new('indicatorComparison',sapply(v[!is_numeric & !is_array],Id),call=sys.call(2))
    } else {
      NULL
    }
    out <- list(numeric=u,nonnumeric=v,array=w)
    out[!sapply(out,is.null)]
})

matches <- function(x,y,id=NULL){
  all(dim(x)==dim(y)) && 
    all(names(x) == names(y)) &&
    ifelse(is.null(id),TRUE, all(x[,id]==y[,id]))
}


### CELLS ----


setClass('cellComparison',contains='comparison')

#' Cell counts and differences for a series of datasets
#'
#' @section Comparing datasets cell by cell:
#'
#' When comparing the contents of two data sets, the total number of cells
#' in the current data set can be partitioned as in the following figure.
#'
#' \if{html}{\figure{cellsplit.png}{options: width=80\% alt="rulewise splitting"}}
#' \if{latex}{\figure{cellsplit.pdf}{options: width=13cm}}
#'
#' This function computes the partition for two or more
#' datasets, comparing the current set to the first (default) or to the 
#' previous (by setting \code{compare='sequential'}).
#'
#' @section Details:
#' This function assumes that the datasets have the same dimensions and that both
#' rows and columns are ordered similarly.
#' 
#' @references
#' The figure is reproduced from MPJ van der Loo and E. De Jonge (2018)
#' \emph{Statistical Data Cleaning with applications in R} (John Wiley & Sons).
#'
#' @param ... For \code{cells}: data frames, comma separated. Names will become
#'    column names in the output. For \code{plot} or \code{barplot}: graphical parameters
#'    (see \code{\link[graphics]{par}}).
#' @param .list A \code{list} of data frames; will be concatenated with 
#'    objects in \code{...}
#' @param compare How to compare the datasets.
#' 
#' 
#' @return An object of class \code{cellComparison}, which is really an array 
#'   with a few extra attributes. It counts the total number of cells, the number of 
#'   missings, the number of altered values and changes therein as compared to 
#'   the reference defined in \code{how}.
#'
#' @family comparing
#' @example ../examples/cells.R
#' @export 
cells <- function(..., .list = NULL, compare=c("to_first","sequential")){

  how <- match.arg(compare)
  L <- c( list(...), .list )
  names(L) <- make_listnames(L)

  out <- if ( how == "to_first" ){
    cbind(  
      cell_diff(L[[1]])
      , vapply( seq_len(length(L)-1)
          , function(i) cell_diff(L[[i+1]], L[[1]])
          , FUN.VALUE = numeric(9))
    )
  } else {
    cbind(
      cell_diff( L[[1]] )
    , vapply( seq_len(length(L)-1)
        , function(i) cell_diff(L[[i+1]], L[[i]])
        , FUN.VALUE = numeric(9) )
    )
  }

  colnames(out) <- names(L)
  new("cellComparison"
   , out
   , call = match.call( definition=cells, sys.call(sys.parent(1L)) )
  )
}

cell_diff <- function(new, old=NULL){

  n_cells <- prod(dim(new))
  n_avail <- sum(!is.na(new))
  n_miss  <- sum( is.na(new))
  
  if (is.null(old)){
    c(
        cells           = n_cells
      , available       = n_avail
      , still_available = sum(!is.na(new))
      , unadapted       = sum(!is.na(new))
      , adapted         = 0
      , imputed         = 0
      , missing         = n_miss
      , still_missing   = sum(is.na(new))
      , removed         = 0
    )
  } else {
    c(
        cells           = n_cells
      , available       = n_avail
      , still_available = sum(!is.na(new) & !is.na(old) )
      , unadapted       = sum( old == new, na.rm=TRUE   )
      , adapted         = sum( old != new, na.rm=TRUE   )
      , imputed         = sum( is.na(old) & !is.na(new) )
      , missing         = n_miss
      , still_missing   = sum( is.na(old) &  is.na(new) )
      , removed         = sum(!is.na(old) &  is.na(new) )
    )
  }
}

#' Translate cellComparison objects to data frame
#'
#' Versions of a data set can be cellwise compared using
#' \code{\link{cells}}. The result is a \code{cellComparison} object, 
#' which can usefully be translated into a data frame.
#'
#' @inheritParams as.data.frame
#'
#' @return A data frame with the following columns.
#' \itemize{
#'  \item{\code{status}: Row names of the \code{cellComparison} object.}
#'  \item{\code{version}: Column names of the \code{cellComparison} object.}
#'  \item{\code{count}: Contents of the \code{cellComparison} object.}
#' }
#'  
#' @example ../examples/cells.R
#' @family comparing
#' @export
setMethod("as.data.frame","cellComparison", function(x,...){
  x <- x[,]
  class(x) <- "table"
  setNames(as.data.frame(x,...),c("status","version","count"))
})


#' Line graph of a cellComparison object.
#'
#' Versions of a data set can be compared cell by cell
#' using \code{\link{cells}}. The result is a \code{cellComparison}
#' object. This method creates a line-graph, thus suggesting an
#' that an ordered sequence of data sets have been compared.
#' See also \code{\link{barplot,cellComparison-method}} for an
#' unordered version.
#'
#' @param x a \code{cellComparison} object.
#' @inheritParams plot,validatorComparison-method
#' @family comparing
#' @export
setMethod("plot","cellComparison"
  , function(x
    , xlab=""
    , ylab=""
    , las=2
    , cex.axis=0.8
    , cex.legend=0.8,...){



  oldpar <- par(mar=c(3,3,3,8))
  on.exit(par(oldpar))
  
  status <- rownames(x)
  version <- colnames(x)
  dat <- as.data.frame(x)
  
  cl_map <- lw_map <- lt_map <- setNames(rep(1,9), rownames(x))
  lt_map[c("adapted","imputed","removed")] <- 2
  
  
  lw_map[1:9] <- 1
  lw_map[c("cells","available","missing","imputed")] <- 2
  
  # Colors taken from RColorBrewer::brewer.pal(8,"Paired")
  cl_map["cells"] <- "black"
  cl_map[c("available","still_available")]    <- "#1F78B4" # dark blue
  cl_map[c("unadapted","adapted","imputed")]  <- "#A6CEE3" # light blue 
  cl_map["missing"] <- "#FF7F00"                           # dark yellow
  cl_map[c("removed","still_missing")] <- "#FDBF6F"        # light yellow
  
  n <- length(version)
  plot(0,0,col='white'
       , xlim=c(1,length(version))
       , ylim=c(0,max(x))
       , las=las
       , xaxt='n'
       , xlab=xlab
       , ylab=ylab
       , cex.axis=cex.axis,...)
  abline(v=seq_along(version),col="grey",lt=3)
  for (stat in status){
    d <- dat[dat$status == stat, ]
    lines( as.integer(d$version), d$count, type='b', col=cl_map[stat]
        ,lw = lw_map[stat], lty = lt_map[stat], pch = 16)  
  }
  axis(side=1,labels=version,at=seq_along(version),
       las=1,padj=rep(c(0,1),times=n),cex.axis=cex.axis)
  oldpar <- c(oldpar,par(xpd=TRUE))
  legend(x=1.04*n,y=max(x)
         , legend = gsub("_"," ",status)
         , lwd = lw_map[status]
         , lty = lt_map[status]
         , col = cl_map[status]
         , cex = cex.legend
         , bty = "n"
         , title="Count"
  )
  invisible(NULL)
})


#' Barplot of cellComparison object
#'
#' Versions of a data set can be compared cell by cell using \code{\link{cells}}.
#' The result is a \code{cellComparison} object. This method creates a stacked bar
#' plot of the results.  See also \code{\link{plot,cellComparison-method}} for a
#' line chart.
#'
#' @param height  object of class \code{cellComparison}
#' @param las [\code{numeric}] in \code{{0,1,2,3}} determining axis label rotation
#' @param cex.axis [\code{numeric}] Magnification with respect to the current
#'   setting of \code{cex} for axis annotation.
#' @param cex.legend [\code{numeric}] Magnification with respect to the current
#'   setting of \code{cex} for legend annotation and title.
#' @param wrap [\code{logical}] Toggle wrapping of x-axis labels when their width
#'  exceeds the width of the column.
#' @param ... Graphical parameters passed to \code{\link[graphics]{barplot.default}}.
#'
#' @note Before plotting, underscores (\code{_}) and dots (\code{.}) in x-axis 
#' labels are replaced with spaces.
#' 
#' @family comparing
#' @export
setMethod("barplot", "cellComparison", function(height
    , las = 1
    , cex.axis = 0.8
    , cex.legend = cex.axis
    , wrap = TRUE
    , ...){

  oldpar <- par(mar=c(3,3,3,8), xpd=TRUE)
  on.exit(par(oldpar))
  # turn into array
  a <- height[,,drop=FALSE]
  a <- a[c("unadapted","adapted","imputed","still_missing","removed"),,drop=FALSE]

  # Colors taken from RColorBrewer::brewer.pal(8,"Paired")
  cl_map <- c(
   unadapted       = "#4575B4" # dark blue
   , adapted       = "#91BFDB" # blue
   , imputed       = "#E0F3F8" # light blue
   , still_missing = "#FC8D59" # orange
   , removed       = "#FEE090" # yellow
  )

  x <- barplot(a
      , col=cl_map[rownames(a)]
      , las=las
      , xaxt="n"
      , cex.axis=cex.axis
      , ...
  )
  # replace punctuation with spaces
  xlabs <- trimws(gsub("[_.]+"," ", colnames(a)))
  # simple wrapping heuristic
  if ( isTRUE(wrap) ){
    barwidth <- if ( length(x) == 1) 1.0 else x[length(x)] - x[length(x)-1] - 0.2
    i <- strwidth(xlabs) > barwidth
    ncol <- ceiling(barwidth/strwidth("m"))
    # wrap and fold
    xlabs[i] <- sapply(xlabs[i]
      , function(s) paste(strwrap(s, width=ncol),collapse="\n") 
    )
    
  }
  axis(side=1, labels=xlabs, at=x, cex.axis=cex.axis,lwd=0)

  # compute legend position
  leg_pos <- bp_leg_pos(x)

  legend(x = leg_pos, y = sum(a[,1])
       , legend= rev(sub("_"," ", rownames(a)))
       , fill=rev(cl_map[rownames(a)]),bty="n"
       , title="Count", cex=0.8)
  invisible(x)  
})

# helper function: compute reasonable location of
# legend in barplot.
bp_leg_pos <- function(x){
  n <- length(x)
  # the factor 1.04 is the default location of the
  # box after the end of the scale.
  leg_pos <- if (n ==1){
      # 1.2 is default width for n==1
      1.04 * 1.2
  } else {
      # bar width + 0.2 (default) separation
   1.04 * (x[n] + (x[n] - x[n-1]-0.2)/2)
  }

}








#' Create matching subsets of a sequence of data
#'
#' @param ... A sequence of \code{data.frame}s, possibly in the form of \code{<name>=<value>} pairs.
#' @param .list A list of \code{data.frame}s; will be concatenated with \code{...}.
#' @param id Names or indices of columns to use as index.
#'
#' @return A list of \code{data.frames}, subsetted and sorted so that all cells correspond.
#' @export
#' @family comparing
match_cells <- function(...,.list=NULL,id=NULL){
  L <- c(list(...), .list)
  
  # match columns
  nm <- Reduce(intersect,lapply(L,names))
  L <- lapply(L,`[`,nm)
  
  if (!is.null(id)){ # match rows
    ID <- lapply(L, function(d) do.call(paste0,as.list(d[id])))
    ids <- Reduce(intersect, ID)
    L <- lapply(seq_along(L), function(i) L[[i]][match(ids, ID[[i]],nomatch=0),]) 
  }
  L
}


