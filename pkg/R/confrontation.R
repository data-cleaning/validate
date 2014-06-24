#' @include indicator.R
#' @include validator.R
NULL

# superclass for storing results of a verification activity
setRefClass("confrontation"
  ,fields = list(
      call  = "call"  # (user's) call that generated the object
    , value = "list"  # results of confrontation 
    , calls = "list"  # calls executed during confrontation
    , warn  = "list"  # list of 'warning' objects
    , error = "list"  # list of 'error' objects
  )
  , methods=list(
    show = function() .show_confrontation(.self)
  )
)

.show_confrontation <- function(.self){
  cat(sprintf("Reference object of class '%s'\n",class(.self)))
  cat(sprintf("Call:\n    ")); print(.self$call); cat('\n')
  cat(sprintf('Confrontations: %d\n', length(.self$calls)))
  cat(sprintf('Warnings      : %d\n',sum(sapply(.self$warn,function(w)!is.null(w)))))
  cat(sprintf('Errors        : %d\n',sum(sapply(.self$error,function(w)!is.null(w)))))
}


#' Confront data with a (set of) verifier(s)
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
setRefClass("indicatorValue", contains = "confrontation")

#' @rdname confront
setMethod("confront",signature("indicator","data"),function(x,y,...){
  calls <- calls(x)
  L <- execute(calls,y)
  new('indicatorValue',
      call = match.call()
      , calls = calls[!is.assignment(calls)]
      , value = lapply(L,"[[",1)
      , warn =  lapply(L,"[[",2)
      , error = lapply(L,"[[",3)     
  )  
})

#' @rdname confront
setMethod('summary',signature('indicatorValue'),function(object,...){
  data.frame(
    indicator = names(object$value)
    , confrontations = sapply(object$value,length)
    , class = get_stat(object,class)
    , min = get_stat(object,min,na.rm=TRUE)
    , mean  = get_stat(object,mean,na.rm=TRUE)
    , max = get_stat(object,max,na.rm=TRUE)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , call = sapply(object$calls,call2text)
    ,row.names=NULL
    ,stringsAsFactors=FALSE
  )  
})

#' @rdname select
#' @export 
setMethod('[',signature('confrontation'),function(x,i,j,...,drop=TRUE){
  new('confrontation',
      , call = x$call[i]
      , value = x$value[i]
      , warn = x$warn[i]
      , error  = x$error[i]
  )
})

# # indicators serve a different purpose than validations.
setRefClass("validatorValue", contains = "confrontation")

#' @rdname confront
setMethod("confront", signature("validator","data"), function(x, y,  ...){
  calls <- calls(x)
  L <- execute(calls,y)
  new('validatorValue',
      call = match.call()
      , calls = calls[!is.assignment(calls)]
      , value = lapply(L,"[[",1)
      , warn =  lapply(L,"[[",2)
      , error = lapply(L,"[[",3)     
  )
})

# execute calls. 
# - Assignments are stored in a separate environment and forgotten afterwards.
# - Failed assignments yield a warning.
execute <- function(calls,env){
  w = new.env()
  lapply(calls, function(g) 
    if (g[[1]] == ":=") 
      w[[as.character(left(g))]] <- tryCatch( eval(right(g), env), error=warning)
    else 
      factory(eval)(g,env,w)
  )[!is.assignment(calls)]
}

has_error <- function(x) !sapply(x$error,is.null)
has_warning <- function(x) !sapply(x$warn, is.null)
has_value <- function(x) sapply(x$value, function(a) !is.null(a))

passes <- function(x){
  sapply(x$value, function(a){
    ifelse( is.null(a), 0, sum(a,na.rm=TRUE)) 
  })
}

fails <- function(x){
  sapply(x$value, function(a){
    ifelse(is.null(a),0,sum(!a,na.rm=TRUE))
  })
}

nas <- function(x){
  sapply(x$value, function(a){
    ifelse(is.null(a),0,sum(is.na(a)))
  })
}

#' @export 
setGeneric('summary')

#' @rdname confront
setMethod('summary',signature('validatorValue'),function(object,...){
  data.frame(
    validator = names(object$value)
    , confrontations = sapply(object$value,length)
    , passes = passes(object)
    , fails  = fails(object)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , call = sapply(object$calls,  call2text)
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
  x$value
})

#' @rdname values
#' @param simplify Combine results with similar dimension structure into arrays?
setMethod('values',signature('validatorValue'),function(x,simplify=TRUE,...){
  if (!simplify ){
    return( getMethod(values,signature='confrontation')(x,...) )
  }
  values <- x$value[!has_error(x)]
  simplify_list(values)
})

#' Get severity values
#' @rdname values
#' @export
setGeneric('severity',def=function(x,...) standardGeneric('severity'))

#' @rdname values
setMethod('severity', signature('validatorValue'),function(x,...){
  values <- x$value[!has_error(x)]
  lists <- sapply(values,is.list)
  L <- lapply(values[lists],function(x) x$severity) 
  simplify_list(L)
})

#' Get impact values
#' @rdname values
#' @export
setGeneric('impact',def=function(x,...) standardGeneric('impact'))

#' @rdname values
setMethod('impact', signature('validatorValue'),function(x,...){
  values <- x$value[!has_error(x)]
  lists <- sapply(values,is.list)
  L <- lapply(values[lists],function(x) x$impact) 
  simplify_list(L)
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

#' @rdname calls
setMethod('calls',signature('confrontation'),function(x, ...){
  x$calls
})

#' @rdname calls
setMethod('calls',signature('validatorValue'), function(x, ...){
  calls <- x$calls[!has_error(x)]
  len <- sapply(x$value[!has_error(x)],length)
  lapply(unique(len),function(l) sapply(calls[len==l],Id))
})
