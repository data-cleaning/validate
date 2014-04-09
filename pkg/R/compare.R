#' @include verifier.R 
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
#' The purpose of this function is to compare different versions of the same dataset with respect
#' to predifined indicators. It is expected that the number and order of columns and rows are the 
#' identical for each dataset. 
#'
#' @param x An R object (usually a \code{\link{validator}} or \code{\link{indicator}}
#' @param ... (named) data sets (\emph{e.g.} data.frames)
#' 
#' @export
setGeneric('compare', def = function(x,...) standardGeneric('compare'))


# @method compare validator
# @param how how to compare
# @param .list Optional list of data sets, will be concatenated with \code{...}.
# @rdname compare
# @export 

setMethod('compare', signature('validator'), 
  function(x, ..., how=c('to_first','sequential'), .list=NULL){
    L <- c( list(...), .list)
    if ( length(L) < 2 ) error('you need at least two datasets')
    how <- match.arg(how)

    out <- if (how == 'to_first'){
        ref <- confront(using,L[[1]])
        vapply(L, function(x) compare2(confront(using, x), ref)
               ,FUN.VALUE=numeric(11))
      } else {
        ref <- NULL
        vapply(seq_along(L), function(i){
          j <- ifelse(i==1,1,i-1)
          compare2( confront(using,L[[i]]),confront(using,L[[j]]) )
        }
        , FUN.VALUE = numeric(11) )
    }
    names(dimnames(out)) <- c('Status','Version')
    new('validatorComparison',
        provideDimnames(out,base=sprintf("D%04d",1:ncol(g)))
        ,call=sys.call(1L)
    )
})



## INTERNAL helper-outers

# y : object to compare against x
# x : reference object to compare against (note the order!)
setGeneric('compare2',def=function(y,x,...) standardGeneric('compare2'))

setMethod('compare2',signature('validatorValue','validatorValue'),
  function(y,x,...){
    vx <- values(x)
    vy <- values(y)
    rules <- errcheck(x,y)
    vx <- lapply(vx, function(x) x[,rules,drop=FALSE])
    vy <- lapply(vy, function(x) x[,rules,drop=FALSE])    
    
    validations      = rep( sum(sapply(vx, length)), 2)
    unverifiable     = c(unverifiable(vx), unverifiable(vy))
    verifiable       = validations-unverifiable
    still_verifiable = c(verifiable[1],still_verifiable(vx,vy))
    new_verifiable   = verifiable - still_verifiable
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
      , still_verifiable   
      , new_verifiable     
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
      , 'still_verifiable'   
      , 'new_verifiable'     
      , 'satisfied'          
      , 'still_satisfied'    
      , 'new_satisfied'      
      , 'violated'           
      , 'still_violated'     
      , 'new_violated'       
      ))
    )[2,]
})

# v : values('validatorValue')
unverifiable <- function(x){
  sum(sapply(x,function(y) sum(is.na(y))))
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



