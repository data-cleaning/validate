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
#' @param .files A character vector of file locations (see section on file parsing below).
#'
#' @section Validating expressions:
#' A \emph{validating expression} is an expression whose evaluation results in \code{TRUE}, \code{FALSE}
#' or \code{NA}. 
#' 
#' @section File parsing:
#' A validation file can include other files by adding one or more \code{# @@validate include <filename>} statements. 
#' Such a statement may not span more than a single line and one include statement may contain only a single file. 
#' Files are sought in R's current working directory unless the full path is given. Files that are included may include 
#' other files as well. Include statements determine the order in which files are parsed: if file A includes file B then 
#' file B is parsed before file A. If file A includes files B and C in that order, then first file B, then file C and 
#' finally file A is parsed. Although it will not matter for the result, for readability reasons it is advisable to 
#' write include statemens at the top of validation files.
#' 
#' 
#'
#' @seealso \code{\link{syntax}}, \code{\link{confront}}
#' 
#' @return \code{validator} object. Use this object to check/{\code{\link{confront}}}
#' data for validity.
#'
#' @example ../examples/validator.R
#' @export
validator <- function(...,.files=NULL) new('validator',..., .files=.files)

setRefClass("validator"
  , contains = 'expressionset'
  , methods = list(
    initialize = function(..., .files=NULL)  ini_validator(.self,...,.files=.files)
    , is_linear = function() sapply(.self$._calls,linear)
      # extra argument: normalize=TRUE
    , linear_coefficients = function(...) get_linear_coefficients(.self, ...) 
  )
)


ini_validator <- function(.self, ..., .files){
  ini_expressionset(.self,..., .files=.files)
  if (length(.self$._calls)==0) return(.self)

  i <- sapply(.self$._calls, function(x) validating(x) || vargroup(x))
  if ( !all(i) ){
    warning(paste(
      "The following rules contain invalid syntax and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$._calls[!i],deparse), 'from', .self$._origin[!i], collapse="\n ")))
  } 
  .self$._calls  <- .self$._calls[i]
  .self$._origin <- .self$._origin[i]
  .self
}


# Extract linear coeffiecients from linear expressions
#
# @section Details: Linear expressions are expressions of the form \eqn{\boldsymbol{Ay}} or
# \eqn{\boldsymbol{Ay}\odot\boldsymbol{b}}, where \eqn{\odot\in\{<,\leq,=,\geq,>\}}.
# This function uses \code{\link{is_linear}} to find linear expressions in \code{x} and returns
# the corresponding coefficients and possibly the operators. 
#
# @param x An R object
# @param ... Arguments to be passed to other methods
#
# @return A list, containing matrix \eqn{\boldsymbol{A}}, and where possible matrix \eqn{\boldsymbol{b}} 
#  and a vector with comparison operators.
#
get_linear_coefficients <- function(x, normalize=TRUE,...){
  
  calls <- x$._calls[x$is_linear()]
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
  
}




