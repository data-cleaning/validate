#' @include verifier.R
NULL

# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.

#' Define indicators
#' 
#' @param ... A comma-separated list of indicators
#' @param files A character vector of file locations
#' 
#' @seealso \code{\link{syntax}}
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
  if (length(.self$calls)==0) return(.self)
  
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
