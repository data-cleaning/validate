#' @include verifier.R
NULL

#' Define validation rules
#'
#' @param ... A comma-separated list of validation expressions
#' @param files A character vector of file locations
#'
#' @export
validator <- function(...,files=NULL) new('validator',...,files=files)

setRefClass("validator"
  , contains = 'verifier'
  , methods = list(
    initialize = function(...,files=NULL)  .validator(.self,...,files=files)
  )
)


.validator <- function(.self, ..., files){
  .verifier(.self,...,files=files)
  if (length(.self$calls)==0) return(.self)

  i <- sapply(.self$calls, function(x) validating(x) || vargroup(x))
  if ( !all(i) ){
    warning(paste(
      "The following rules contain invalid syntax and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  } 
  .self$calls  <- .self$calls[i]
  .self$origin <- .self$origin[i]
  .self
}

# @method is_linear validator
# @rdname is_linear
# @exportMethod
setMethod("is_linear",signature("validator"), function(x,...){
  sapply(x$calls, linear)
})

# @method linear_coefficients validator
# @param normalize Bring all equations in the \eqn{<} or \eqn{\leq} form. 
# @rdname linear_coefficients
# @export
setMethod("linear_coefficients", signature("validator"),function(x, normalize=TRUE,...){
  
  calls <- x$calls[is_linear(x)]
  cols <- unique(unlist(lapply(calls, var_from_call)))
  rows <- names(calls)
  
  bA <- matrix(0
               , nrow = length(rows)
               , ncol = length(cols) + 1
               , dimnames = list(validator=rows, variable=c('CONSTANT',cols) )
  )
  
  lcoef <- lapply(calls, function(x) coefficients(left(x)))
  rcoef <- lapply(calls, function(x) coefficients(right(x)))
  
  for ( i in seq_along(lcoef) ){
    cls <- names(lcoef[[i]])
    bA[i,cls] <- lcoef[[i]]
    cls <- names(rcoef[[i]])
    bA[i,cls] <- bA[i,cls] - rcoef[[i]]
  }
  
  operators <- sapply(sapply(calls,`[[`,1),deparse)
  
  if (normalize){
    bA <- bA * operatorsign[operators]
    operators <- normed_operators[operators]
  }
  
  list(A=bA[,-1,drop=FALSE],b = -1*bA[,1,drop=FALSE],operators=operators)
  
})


