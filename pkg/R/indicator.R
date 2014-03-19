#' @include verifier.R
NULL

cat("I AM INDICATOR\n")


# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.

indicator <- setRefClass("indicator", contains='verifier',
  methods = list(
    initialize = function(..., files=NULL) .indicator(.self,...,files=files)
  )                       
)


.indicator <- function(.self,...,files){
  .verifier(.self,...,files=files)
  
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



