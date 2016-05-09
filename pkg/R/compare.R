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
#' 
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
setMethod('compare', 'validator', 
  function(x, ..., .list=NULL, how=c('to_first','sequential') ){
    L <- c( list(...), .list)
    if ( length(L) < 2 ) stop('you need at least two datasets')
    how <- match.arg(how)
    names(L) <- make_listnames(L)
    
    out <- if (how == 'to_first'){
        ref <- confront(L[[1]],x)
        vapply(L, function(y) compare2(confront(y, x), ref)
               ,FUN.VALUE=numeric(11))
      } else {
        ref <- NULL
        vapply(seq_along(L), function(i){
          j <- ifelse(i==1,1,i-1)
          compare2( confront(L[[i]],x),confront(L[[j]],x) )
        }
        , FUN.VALUE = numeric(11) )
    }
    names(dimnames(out)) <- c('Status','Version')
    new('validatorComparison',
        provideDimnames(out,base=sprintf("D%04d",1:ncol(out)))
        ,call=sys.call(1L)
    )
})



## INTERNAL helper-outers

#
# y : object to compare against x
# x : reference object to compare against (note the order!)
setGeneric('compare2',def=function(y,x,...) standardGeneric('compare2'))

setMethod('compare2',signature('validation','validation'),
  function(y,x,...){
    vx <- values(x,drop=FALSE)
    vy <- values(y,drop=FALSE)
    rules <- errcheck(x,y)
    vx <- lapply(vx, function(x) x[,rules,drop=FALSE])
    vy <- lapply(vy, function(x) x[,rules,drop=FALSE])    
    
    validations      = rep( sum(sapply(vx, length)), 2)
    unverifiable     = c(unverifiable(vx), unverifiable(vy))
    verifiable       = validations-unverifiable
    still_unverifiable = c(unverifiable[1],still_unverifiable(vx,vy))
    new_unverifiable = unverifiable - still_unverifiable
    satisfied        = c(satisfied(vx),satisfied(vy))
    still_satisfied  = c(satisfied[1], still_satisfied(vx,vy))
    new_satisfied    = satisfied - still_satisfied
    violated         = verifiable - satisfied
    still_violated   = c(violated[1],still_violated(vx,vy))
    new_violated     = violated - still_violated

    array(c(
       validations           
      , verifiable         
      , unverifiable       
      , still_unverifiable   
      , new_unverifiable     
      , satisfied          
      , still_satisfied    
      , new_satisfied      
      , violated           
      , still_violated     
      , new_violated       
    ) , dim=c(2,11)
      , dimnames=list(
          NULL
        , status = c(
        'validations'           
      , 'verifiable'         
      , 'unverifiable'       
      , 'still_unverifiable'   
      , 'new_unverifiable'     
      , 'satisfied'          
      , 'still_satisfied'    
      , 'new_satisfied'      
      , 'violated'           
      , 'still_violated'     
      , 'new_violated'       
      ))
    )[2,]
})

# v : values('validation')
unverifiable <- function(x){
  sum(sapply(x,function(y) sum(is.na(y))))
}

still_unverifiable <- function(x,y){
  sum(sapply(seq_along(x), function(i) sum(is.na(x[[i]]) & is.na(y[[i]]))) )
}

# y wrt x
still_verifiable <- function(x,y){
  sum(sapply(seq_along(x), function(i) sum(!is.na(x[[i]]) & !is.na(y[[i]]))))
}

new_verifiable <- function(x,y){
   sum(sapply(seq_along(x),function(i) sum(is.na(x[[i]]& !is.na(y[[i]])))))
}

satisfied <- function(x){
  sum(sapply(x,sum,na.rm=TRUE))
}

still_satisfied <- function(x,y){
  sum(sapply(seq_along(x), function(i) sum(x[[i]] & y[[i]],na.rm=TRUE)))
}

still_violated <- function(x,y){
  sapply(seq_along(x), function(i) sum(!x[[i]] & !y[[i]],na.rm=TRUE))
}

errcheck <- function(x,y){
  hex <- has_error(x)
  hey <- has_error(y)
  if (!all(hex == hey)){
    xnoty <- paste(names(hex & !hey),collapse=", ")
    ynotx <- paste(names(!hex & hey),collapse=", ")
    warning(
      sprintf(
        'Not every call could be evaluated in both datasets:\n x not y :%s\n y not x%s\n'
        , xnoty, ynotx)
      )
  }
  which(!hex & !hey)
}

# See table 4 in Van den Broek, Van der Loo and Pannekoek
setMethod('compare2',signature('data.frame','data.frame'),function(y,x,...){
  stopifnot(dim(x)==dim(y))
  n <- rep(prod(dim(x)),2)
  available       <- c(sum(!is.na(x)),sum(!is.na(y)))
  still_available <- c(available[1], sum(!is.na(x)&!is.na(y)) )
  unadapted       <- c(still_available[1], sum(x == y,na.rm=TRUE))
  adapted         <- still_available - unadapted
  imputed         <- c(0,sum(is.na(x)&!is.na(y)))
  missing         <- n - available
  new_missing     <- c(0,sum(!is.na(x) & is.na(y)))
  still_missing   <- c(missing,missing - new_missing)

  array(c(
      n
    , available
    , missing
    , still_available
    , unadapted
    , adapted
    , imputed
    , new_missing
    , still_missing
  )
  , dim = c(2,9)
  , dimnames=list(NULL,
   status = c(
     'cells'
     ,'available'
     ,'missing'
     ,'still_available'
     ,'unadapted'
     ,'adapted'
     ,'imputed'
     ,'new_missing'
     ,'still_missing'
     ))
  )[2,]
})

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
cells <- function(...,.list=NULL, compare=c('to_first','sequential')){
  L <- c( list(...), .list)
  if ( length(L) < 2 ) stop('you need at least two datasets')
  how <- match.arg(compare)
  names(L) <- make_listnames(L)
    
  new('cellComparison',
      if ( how == 'to_first'){
        vapply(L,FUN=compare2,FUN.VALUE=numeric(9),x=L[[1]])
      } else { 
        v <- vapply(seq_along(L)
              , FUN = function(i){
                j = ifelse(i==1,1,i-1)     
                compare2(L[[j]],L[[i]])
                }, FUN.VALUE=numeric(9)
        )
        colnames(v) <- names(L)
        v
      }
    , call=sys.call()
  )
}

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


