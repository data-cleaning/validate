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
#' @param .file A character vector of file locations (see also the section on file parsing in the 
#' \code{\link{syntax}} help file).
#'
#' @section Validating expressions:
#' A \emph{validating expression} is an expression whose evaluation results in \code{TRUE}, \code{FALSE}
#' or \code{NA}. 
#' 
#'  
#'
#' @seealso 
#' \itemize{
#'  \item{\code{\link{syntax}}} 
#'  \item{\code{\link{confront}}}, \code{\link{check_that}}
#'  \item{\code{\link{summary,expressionset-method}}}
#'  \item{\code{\link{validator-class}}}
#' }
#' @return \code{validator} object. Use this object to check/{\code{\link{confront}}}
#' data for validity.
#'
#' @example ../examples/validator.R
#' @export
validator <- function(..., .file) new('validator',..., .file = .file)

#### VALIDATOR CLASS ----------------------------------------------------------

#' Store a set of rich validating rules.
#'
#' @section Details:
#' A validator stores a set of validatin rules. It is a child class of \code{\link{expressionset}} and
#' can be constructed with  \code{\link{validator}}.
#'
#' @section Exported S4 methods for \code{validator}:
#'   \itemize{
#'  \item{Methods inherited from \code{\link{expressionset}}}
#'  \item{\code{\link{confront}}}
#'  \item{\code{\link{compare}}}
#' }
#' 
#'
#' @section See also:
#' \itemize{
#'  \item{\code{\link{validator}}}
#'  \item{\code{\link{expressionset}}}
#' }
#'
#' @keywords internal
#' 
setRefClass("validator"
  , contains = 'expressionset'
  , methods = list(
    initialize = function(..., .file)  ini_validator(.self,...,.file=.file)
    , is_linear = function() linear(.self)
      # extra argument: normalize=TRUE
    , linear_coefficients = function(...) get_linear_coefficients(.self, ...) 
  )
)

ini_validator <- function(obj, ..., .file){
  
  if (missing(.file)){
    ini_expressionset_cli(obj, ..., .prefix="V")
    obj$._options <- PKGOPT
    i <- validating(obj)
    if ( !all(i) ){
      not_validating <- sapply(which(!i),function(k) deparse(expr(obj[[k]])))
      wrn <- sprintf("\n[%03d] %s",which(!i),not_validating)
      warning(paste0(
        "Invalid syntax detected, the following expressions have been ignored:"
        , paste0(wrn,collapse="")
        ))
      obj$rules <- obj$rules[i]
    } 
  } else {
    ini_expressionset_yml(obj, file=.file, .prefix="V")
  }
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
  x <- x[x$is_linear()]
  calls <- get_exprs(x)
    
  cols <- unique(unlist(lapply(calls, var_from_call)))
  rows <- names(x)
  
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








