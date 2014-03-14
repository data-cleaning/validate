
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

# confront a verifier with a (set of) data set(s)
setMethod("confront",signature("verifier","data"), function(x,y,...){
  L <- lapply(x$calls,factory(eval), y)
  new('confrontation',
      call = match.call()
      , calls = x$calls
      , value = lapply(L,"[[",1)
      , warn  = lapply(L,"[[",2)
      , error = lapply(L,"[[",3)     
  )
})


# indicators serve a different purpose than validations.
setRefClass("indicatorValue", contains = "confrontation")

setMethod("confront",signature("indicator","data"),function(x,y,...){
  L <- lapply(x$calls,factory(eval), y)
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

## TODO: parse x$y --> x[,'y'] so R reports error when 'y' doesn't exist. 
setMethod("confront", signature("validator","data"),
  function(x, y
    , impact=c("none","Lp","rspa","FH")
    , severity=c("none","Lp","gower")
    , p=c(impact=2,severity=1), ...)
  {
    calls <- calls(x)
    L <- lapply(calls,factory(eval), y)
    new('validatorValue',
        call = match.call(call=sys.call(1))
        , calls = calls
        , value = lapply(L,"[[",1)
        , warn =  lapply(L,"[[",2)
        , error = lapply(L,"[[",3)     
    )
  }
)

has_error <- function(x) sapply(x$error,is.null)
has_waring <- function(x) sapply(x$warn, is.null)
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

# TODO make
setMethod('summary',signature('validatorValue'),function(object,...){
  data.frame(
    validator = names(object$value)
    , confrontations = sapply(object$value,length)
    , passes = passes(cf)
    , fails  = fails(cf)
    , nNA = nas(cf)
    , error = !sapply(object$error, is.null)
    , warning = !sapply(object$warn, is.null)
    , call = sapply(object$calls,call2text)
  )  
})



