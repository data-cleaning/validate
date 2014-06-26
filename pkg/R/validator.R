#' @include expressionset.R
NULL

#' Define validation rules for data
#'
#' With \code{validator} a set of validation rules can defined, which can be
#' used to \code{\link{confront}} data. \code{validator} is a specific case of
#' \code{expressionset}: a set of rules to verify data. 
#' 
#' @section Validating expressions:
#' Each validating expression should evaluate to a \code{logical}. The syntax of
#' the expression is described in \code{\link{syntax}}. 
#' 
#' @param ... A comma-separated list of validating expressions
#' @param files A character vector of file locations
#'
#' @section Details:
#' A \emph{validating expression} is an expression whose evaluation results in \code{TRUE}, \code{FALSE}
#' or \code{NA}. 
#'
#' @seealso \code{\link{syntax}}, \code{\link{confront}}
#' 
#' @return \code{validator} object. Use this object to check/{\code{\link{confront}}}
#' data for validity.
#'
#' @example ../examples/validator.R
#' @export
validator <- function(...,files=NULL) new('validator',...,files=files)

setRefClass("validator"
  , contains = 'expressionset'
  , methods = list(
    initialize = function(...,files=NULL)  ini_validator(.self,...,files=files)
  )
)


ini_validator <- function(.self, ..., files){
  ini_expressionset(.self,...,files=files)
  if (length(.self$._calls)==0) return(.self)

  i <- sapply(.self$._calls, function(x) validating(x) || vargroup(x))
  if ( !all(i) ){
    warning(paste(
      "The following rules contain invalid syntax and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$._calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  } 
  .self$._calls  <- .self$._calls[i]
  .self$origin <- .self$origin[i]
  .self
}

#' @rdname is_linear
setMethod("is_linear",signature("validator"), function(x,...){
  sapply(x$._calls, linear)
})

#' @param normalize Bring all equations in the \eqn{<} or \eqn{\leq} form. 
#' @rdname linear_coefficients
setMethod("linear_coefficients", signature("validator"),function(x, normalize=TRUE,...){
  
  calls <- x$._calls[is_linear(x)]
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




