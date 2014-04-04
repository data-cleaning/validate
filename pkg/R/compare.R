

setGeneric('compare', def = function(x,...) standardGeneric('compare'))

setMethod('compare',signature=signature('validator'), 
  function(x, reference=c('base','sequential'), ..., .list=NULL){
    L <- c( list(...), .list)
    reference <- match.arg(reference)
    
  
})



## helper-outers

# x : reference object to compare against
# y : object to compare against x
setGeneric('compare2',def=function(x,y,...) standardGeneric('compare2'))

setMethod('compare2',signature('validatorValue','validatorValue'),
  function(x,y,...){
    vx <- values(x)
    vy <- values(y)
    rules <- errcheck(x,y)
    vx <- lapply(vx, function(x) x[,rules,drop=FALSE])
    vy <- lapply(vy, function(x) x[,rules,drop=FALSE])
  #TODO pairwise comparing of elements in vx and vy    
})




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



