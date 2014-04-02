#' @include verifier.R
NULL

# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.

#' Define indicators
#' 
#' @param ... A comma-separated list of indicators
#' @param files A character vector of file locations
#'
#' @export
indicator <- function(...,files=NULL) new('indicator',...,files=files)

setRefClass("indicator", contains='verifier',
  methods = list(
    initialize = function(..., files=NULL) .indicator(.self,...,files=files)
  )                       
)


.indicator <- function(.self,...,files){
  .verifier(.self,...,files=files,prefix="I")
  
  i <- sapply(.self$calls, function(x) !validating(x) || vargroup(x))
  if ( !all(i) ){
    warning(paste(
      "The following rules contain invalid syntax and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  }
  .self$calls  <- .self$calls[i]
  .self$origin <- .self$origin[i]
  .self 
}


get_stat <- function(x,what,...){
  out <- rep(NA,length(x$value))
  i <- !has_error(x)
  out[i] <- tryCatch(
    sapply(x$value[i],what,...)
    , error = function(e) NA
    , warning = function(e) NA
  )
  out
}
#' @method summary validatorValue
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


