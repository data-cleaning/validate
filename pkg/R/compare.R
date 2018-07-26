#' @include validator.R
#' @include indicator.R 
#' @include confrontation.R
NULL

setClassUnion('callOrNull',list('call','NULL'))

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
#' Compare different versions of the same dataset with respect to predifined
#' indicators. Results are simplified in a sensible way.
#'
#' @param x An R object
#' @param ... (named) data sets (\emph{e.g.} data.frames) 
#' @seealso 
#' \itemize{
#'  \item{\code{\link{cells}}}
#'  \item{\code{\link{validator}}, \code{\link{validator-class}}}
#'  \item{\code{\link{indicator}}, \code{\link{indicator-class}}}
#' }
#' 
#' @example ../examples/compare.R
#' @export
setGeneric('compare', def = function(x,...) standardGeneric('compare'))

#' @param how how to compare
#' @param .list Optional list of data sets, will be concatenated with \code{...}.
#' @rdname compare
#' 
#' @return For \code{validator}: An array where each column represents one dataset. The rows count the following
#' attributes:
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
        s <- s + sum(!cf_old[[i]] & cf_new[[i]], na.rm=TRUE)
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
        s <- s + sum(cf_old[[i]] & !cf_new[[i]], na.rm=TRUE)
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





setClass('cellComparison',contains='comparison')

#' Cell counts and differences for a series of datasets
#'
#' @section Details:
#' This function assumes that the datasets have the same dimensions and that both
#' rows and columns are ordered similarly.
#' 
#' 
#' @param ... A (named) sequence of R objects carrying data (\emph{e.g.} \code{data.frame}s)
#' @param .list A list of R objects carrying data; will be concatenated with objects in \code{...}
#' @param compare How to compare the datasets.
#' 
#' 
#' @return An object of class \code{cellComparison}, which is really an array 
#'   with a few attributes. It counts the total number of cells, the number of 
#'   missings, the number of altered values and changes therein as compared to 
#'   the reference defined in \code{how}.
#'
#' @seealso 
#' \itemize{
#'  \item{\code{\link{compare}}} 
#'  \item{\code{\link{match_cells}}}
#' }
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
      , missing         = n_miss
      , still_available = sum(!is.na(new))
      , unadapted       = sum(!is.na(new))
      , adapted         = 0
      , imputed         = 0
      , new_missing     = 0
      , still_missing   = sum(is.na(new))
    )
  } else {
    c(
        cells           = n_cells
      , available       = n_avail
      , missing         = n_miss
      , still_available = sum(!is.na(new) & !is.na(old) )
      , unadapted       = sum( old == new, na.rm=TRUE   )
      , adapted         = sum( old != new, na.rm=TRUE   )
      , imputed         = sum( is.na(old) & !is.na(new) )
      , new_missing     = sum(!is.na(old) &  is.na(new) )
      , still_missing   = sum( is.na(old) &  is.na(new) )
    )
  }
}

#' @inheritParams as.data.frame
#' 
#' @rdname compare
#' @export
setMethod("as.data.frame","validatorComparison", function(x,...){
  x <- x[,]
  class(x) <- "table"
  as.data.frame(x,...)
})

#' @inheritParams as.data.frame
#'  
#' @rdname cells
#' @export
setMethod("as.data.frame","cellComparison", function(x,...){
  x <- x[,]
  class(x) <- "table"
  as.data.frame(x,...)
})

#' Create matching subsets of a sequence of data
#'
#' @param ... A sequence of \code{data.frame}s, possibly in the form of \code{<name>=<value>} pairs.
#' @param .list A list of \code{data.frame}s; will be concatenated with \code{...}.
#' @param id Names or indices of columns to use as index.
#'
#' @return A list of \code{data.frames}, subsetted and sorted so that all cells correspond.
#' @export
#' @seealso
#' \itemize{
#'  \item{\code{\link{cells}}}
#' }
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


#' @param y ignored
#' @rdname compare
#' @export
setMethod("plot", "validatorComparison", function(x,...){
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
   , las     = 2
   , xaxt    = 'n'
   , xlab    = ""
   , ylab    = ""
   , cex.axis=0.8
   , ...)
  # vertical grid
  abline(v = seq_along(version), col = "grey", lty = 3)
  # graph the main lines
  for (stat in status){
    d <- dat[dat$Status == stat, ]
    lines( as.integer(d$Version), d$Freq
       , type='b', col=cl_map[stat]
       , lw=lw_map[stat], lt=lt_map[stat], pch=16)  
  }
  axis(side=1, labels=version, at=seq_along(version)
      , las=1, padj=c(0,1)
      , cex.axis=0.8)
  # add legend
  oldpar <- c(oldpar,par(xpd=TRUE))
  legend(x=1.04*n,y=max(x)
    , legend = gsub("_"," ",status)
    , lwd = lw_map[status]
    , lty = lt_map[status]
    , col = cl_map[status]
    , cex=0.8
    , bty="n"
  )

})
