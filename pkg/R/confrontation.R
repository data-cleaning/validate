#' @include indicator.R
#' @include validator.R
NULL

# superclass for storing results of a verification activity
setRefClass("confrontation"
  ,fields = list(
      ._call  = "call"  # (user's) call that generated the object
    , ._value = "list"  # results of confrontation 
    , ._calls = "list"  # calls executed during confrontation
    , ._warn  = "list"  # list of 'warning' objects
    , ._error = "list"  # list of 'error' objects
  )
  , methods=list(
    show = function() .show_confrontation(.self)
  )
)

.show_confrontation <- function(.self){
  cat(sprintf("Reference object of class '%s'\n",class(.self)))
  cat(sprintf("Call:\n    ")); print(.self$._call); cat('\n')
  cat(sprintf('Confrontations: %d\n', length(.self$._calls)))
  cat(sprintf('Warnings      : %d\n',sum(sapply(.self$._warn,function(w)!is.null(w)))))
  cat(sprintf('Errors        : %d\n',sum(sapply(.self$._error,function(w)!is.null(w)))))
}


#' Confront data with a (set of) expressionset(s)
#'
#' @param x An R object carrying verifications
#' @param y An R object carrying data
#' @param ... Arguments to be passed to other methods
#' @export 
setGeneric("confront",
  def = function(x, y, ...) standardGeneric("confront")
)

setClassUnion('data',c("data.frame","list","environment"))

# indicators serve a different purpose than validations.
setRefClass("indication", contains = "confrontation")

#' @rdname confront
setMethod("confront",signature("indicator","data"),function(x,y,...){
  calls <- x$calls()
  L <- execute(calls,y)
  new('indication',
      ._call = match.call()
      , ._calls = calls[!is.assignment(calls)]
      , ._value = lapply(L,"[[",1)
      , ._warn =  lapply(L,"[[",2)
      , ._error = lapply(L,"[[",3)     
  )  
})

#' @rdname confront
setMethod('summary',signature('indication'),function(object,...){
  data.frame(
    indicator = names(object$._value)
    , confrontations = sapply(object$._value,length)
    , class = get_stat(object,class)
    , min = get_stat(object,min,na.rm=TRUE)
    , mean  = get_stat(object,mean,na.rm=TRUE)
    , max = get_stat(object,max,na.rm=TRUE)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , call = sapply(object$._calls,call2text)
    , row.names=NULL
    , stringsAsFactors=FALSE
  )  
})

#' @rdname select
#' @export 
setMethod('[',signature('confrontation'),function(x,i,j,...,drop=TRUE){
  new(class(x)
      , ._call = match.call()
      , ._calls = x$._calls[i]
      , ._value = x$._value[i]
      , ._warn = x$._warn[i]
      , ._error  = x$._error[i]
  )
})

# # indicators serve a different purpose than validations.
setRefClass("validation", contains = "confrontation")

#' @rdname confront
setMethod("confront", signature("validator","data"), function(x, y,  ...){
  calls <- x$calls()
  L <- execute(calls,y)
  new('validation',
      ._call = match.call()
      , ._calls = calls[!is.assignment(calls)]
      , ._value = lapply(L,"[[",1)
      , ._warn =  lapply(L,"[[",2)
      , ._error = lapply(L,"[[",3)     
  )
})

# execute calls. 
# - Assignments are stored in a separate environment and forgotten afterwards.
# - Failed assignments yield a warning.
execute <- function(calls,env){
  w = new.env()
  lapply(calls, function(g) 
    if (g[[1]] == c(":=")) 
      w[[as.character(left(g))]] <- tryCatch( eval(right(g), env), error=warning)
    else 
      factory(eval)(g, env, w)
  )[!is.assignment(calls)]
}

# x inherits from 'confrontation'
has_error <- function(x) !sapply(x$._error,is.null)
has_warning <- function(x) !sapply(x$._warn, is.null)
has_value <- function(x) sapply(x$._value, function(a) !is.null(a))

passes <- function(x){
  sapply(x$._value, function(a){
    ifelse( is.null(a), 0, sum(a,na.rm=TRUE)) 
  })
}

fails <- function(x){
  sapply(x$._value, function(a){
    ifelse(is.null(a),0,sum(!a,na.rm=TRUE))
  })
}

nas <- function(x){
  sapply(x$._value, function(a){
    ifelse(is.null(a),0,sum(is.na(a)))
  })
}

#' @export 
setGeneric('summary')

#' @rdname confront
setMethod('summary',signature('validation'),function(object,...){
  data.frame(
    validator = names(object$._value)
    , confrontations = sapply(object$._value,length)
    , passes = passes(object)
    , fails  = fails(object)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , call = sapply(object$._calls,  call2text)
  )  
})

#' Get values from object
#' 
#' @aliases severity, impact
#' 
#' @param x an R object
#' @param ... Arguments to pass to or from other methods
#'
#' @export
setGeneric('values',def=function(x,...) standardGeneric('values'))

#' @rdname values
setMethod('values',signature('confrontation'),function(x,...){
  x$._value
})

#' @rdname values
#' @param simplify Combine results with similar dimension structure into arrays?
#' @param drop if a single vector or array results, drop 'list' attribute?
setMethod('values',signature('validation'),function(x,simplify=TRUE,drop=TRUE,...){
  out <- if ( simplify ){
    simplify_list(x$._value[!has_error(x)])
  } else {
    getMethod(values,signature='confrontation')(x,...)
  }
  if (drop && length(out) == 1) out[[1]] else out
})


simplify_list <- function(L){
  len <- sapply(L,num_result)
  lapply(unique(len), function(l){
    m <- sapply(L[len==l], get_result)
    if (l == 1)
      m <- matrix(m,nrow=1,dimnames=list(NULL,names(m)))
    m
  })
}


### currently obsolete stuff ----

# @rdname values
# @export
#setGeneric('severity',def=function(x,...) standardGeneric('severity'))

# @rdname values
# setMethod('severity', signature('validation'),function(x,...){
#   values <- x$._value[!has_error(x)]
#   lists <- sapply(values,is.list)
#   L <- lapply(values[lists],function(x) x$severity) 
#   simplify_list(L)
# })

# @rdname values
# @export
#setGeneric('impact',def=function(x,...) standardGeneric('impact'))

# @rdname values
# setMethod('impact', signature('validation'),function(x,...){
#   values <- x$value[!has_error(x)]
#   lists <- sapply(values,is.list)
#   L <- lapply(values[lists],function(x) x$impact) 
#   simplify_list(L)
# })
