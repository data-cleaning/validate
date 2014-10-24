#' @include expressionset.R
NULL

# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.

#' Define indicators for data
#' 
#' \code{\link{indicator}}
#' @param ... A comma-separated list of indicator definitions
#' @param .files A character vector of file locations
#' 
#' @seealso \code{\link{syntax}}
#' 
#' @export
#' @example ../examples/indicator.R
indicator <- function(..., .files=NULL) new('indicator',..., .files=.files)

setRefClass("indicator", contains='expressionset',
  methods = list(
    initialize = function(..., .files=NULL) ini_indicator(.self,...,.files=.files)
  )                       
)


ini_indicator <- function(.self,...,.files){
  ini_expressionset(.self,..., .files=.files, .prefix="I",.options = PKGOPT)
  if (length(.self$._calls)==0) return(.self)
  
  i <- sapply(.self$._calls, function(x) !validating(x,.self) || vargroup(x))
  if ( !all(i) ){
    warning(paste(
      "The following rules contain invalid syntax and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$._calls[!i],deparse), 'from', .self$._origin[!i], collapse="\n ")))
  }
  .self$._calls  <- .self$._calls[i]
  .self$._origin <- .self$._origin[i]
  .self 
}


get_stat <- function(x,what,...){
  out <- rep(NA,length(x$._value))
  i <- !has_error(x)
  out[i] <- tryCatch(
    sapply(x$._value[i],what,...)
    , error = function(e) NA
    , warning = function(e) NA
  )
  out
}

