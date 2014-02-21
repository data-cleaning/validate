#' Create or manipulate a set of restrictions
#' 
#' @usage restrictionset(...,files=NULL)
#' @param ... A (named) comma-separated list of expressions.
#' @param files A Character vector
#'
#' @return For \code{restrictionset}, a reference object of class 'restrictionset'.
#'
#'
#' @seealso \code{\link{vars}}, \code{\link{validate}}
#' 
#'
#' @exportClass restrictionset
restrictionset <- setRefClass("restrictionset"
  , contains = 'validator'
  , methods = list(
    initialize = function(...,files=NULL)  .restrictionset(.self,...,files=files)    
    )
)


.restrictionset <- function(.self, ..., files){
  .validator(.self,...,files=files)
  
  i <- sapply(.self$calls, is.validating)
  if ( !all(i) ){
    warning(paste(
      "The following rules are not validation rules and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  }
  .self$calls <- parse_restrictions(.self$calls[i])
  .self$origin <- .self$origin[i]
  .self
}

parse_restrictions <- function(x){
  x <- lapply(x,extract_datamodel)
  lapply(x,vectorize)
}

# example
.onLoad()
r <- restrictionset( z+y>=3)
