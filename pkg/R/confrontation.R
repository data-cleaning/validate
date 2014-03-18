
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
  cat(sprintf('Confrontations: %d\n', length(.self$calls)))
  cat(sprintf('Warnings      : %d\n',sum(sapply(.self$warn,function(w)!is.null(w)))))
  cat(sprintf('Errors        : %d\n',sum(sapply(.self$error,function(w)!is.null(w)))))
}


# confront data with a subclass of 'validator'
setGeneric("confront",
  def = function(x, y, ...) standardGeneric("confront")
)

setClassUnion('data',c("data.frame","list","environment"))


# indicators serve a different purpose than validations.
setRefClass("indicatorValue", contains = "confrontation")

setMethod("confront",signature("indicator","data"),function(x,y,...){
  L <- lapply(x$calls,factory(eval), envir=y, enclos=NULL)
  new('indicatorValue',
      call = match.call()
      , calls = x$calls
      , value = lapply(L,"[[",1)
      , warn =  lapply(L,"[[",2)
      , error = lapply(L,"[[",3)     
  )  
})

# indicators serve a different purpose than validations.
setRefClass("validatorValue", contains = "confrontation",
  fields = list(
        impact     = "list" # impact of mismatch on data
      , severity   = "list" # amount of mismatch between actual and desired score
    )
)

setMethod("confront", signature("validator","data"),
  function(x, y
    , impact=c("none","Lp","rspa","FH")
    , severity=c("none","Lp","gower")
    , p=c(impact=2,severity=1), ...)
  {
    calls <- calls(x)
    L <- lapply(calls,factory(eval), envir=y,enclos=NULL)
    new('validatorValue',
        call = match.call(call=sys.call(1))
        , calls = calls
        , value = lapply(L,"[[",1)
        , warn =  lapply(L,"[[",2)
        , error = lapply(L,"[[",3)     
    )
  }
)

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

setMethod('summary',signature('validatorValue'),function(object,...){
  data.frame(
    validator = names(object$value)
    , confrontations = sapply(object$value,length)
    , passes = passes(cf)
    , fails  = fails(cf)
    , nNA = nas(cf)
    , error = has_error(x)
    , warning = has_warning(x)
    , call = sapply(object$calls,call2text)
  )  
})


setGeneric('value',def=function(x,...) standardGeneric('value'))

setMethod('value',signature('confrontation'),function(x,...){
  x$value
})

setMethod('value',signature('validatorValue'),function(x,simplify=TRUE,...){
  if (!simplify ){
    return( getMethod(value,signature='confrontation')(x,...) )
  }
  values <- x$value[!has_error(x)]
  len <- sapply(values,length)
  lapply(unique(len), function(l) sapply(values[len==l],Id))
})

setMethod('calls',signature('confrontation'),function(x,...){
  x$calls
})

setMethod('calls',signature('validatorValue'), function(x,simplify=TRUE,...){
  if (!simplify){
    return( getMethod(calls, signature='confrontation')(x,...) )
  }
  calls <- x$calls[!has_error(x)]
  len <- sapply(x$value[!has_error(x)],length)
  lapply(unique(len),function(l) sapply(calls[len==l],Id))
})



