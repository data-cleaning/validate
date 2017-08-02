#' @include expressionset.R
NULL

#' Define validation rules for data
#' 
#' 
#' @section Validating expressions:
#' Each validating expression should evaluate to a \code{logical}. Allowed syntax of
#' the expression is described in \code{\link{syntax}}. 
#' 
#' @param ... A comma-separated list of validating expressions
#' @param .file (optional) A character vector of file locations (see also the
#'   section on file parsing in the
#' @param .data (optional) A \code{data.frame} with columns \code{"rule"},
#'   \code{"name"}, and \code{"description"}
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
#' @return An object of class \code{validator} (see \code{\link{validator-class}}).
#' 
#'
#' @example ../examples/validator.R
#' @export
validator <- function(..., .file, .data) new('validator',...
            , .file = .file, .data=.data)

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
#'  \item{\code{\link{expressionset}}}
#' }
#'
#' @keywords internal
#' 
setRefClass("validator"
  , contains = 'expressionset'
  , methods = list(
    initialize = function(..., .file, .data)  ini_validator(.self,...,.file=.file, .data=.data)
    , is_linear = function() linear(.self)
      # extra argument: normalize=TRUE
    , linear_coefficients = function(...) get_linear_coefficients(.self, ...) 
  )
)

ini_validator <- function(obj, ..., .file, .data){
  check_primitives()
  if (missing(.file) && missing(.data) ){
    .ini_expressionset_cli(obj, ..., .prefix="V")
    obj$._options <- .PKGOPT
    i <- validating(obj) | is_tran_assign(obj)
    if ( !all(i) ){
      invalid <- sapply(which(!i),function(k) deparse(expr(obj[[k]])))
      wrn <- sprintf("\n[%03d] %s",which(!i), invalid)
      warning(paste0(
        "Invalid syntax detected, the following expressions have been ignored:"
        , paste0(wrn,collapse="")
        ))
      obj$rules <- obj$rules[i]
    } 
  } else if (!missing(.file)) {
    .ini_expressionset_yml(obj, file=.file, .prefix="V")
  } else if (!missing(.data)){
    .ini_expressionset_df(obj, dat=.data, .prefix="V")
    i <- validating(obj) | is_tran_assign(obj)
    if (!all(i)){
      r <- paste(which(!i),collapse=", ")
      warning("Invalid syntax detected, ignoring rows ",r)
      obj$rules <- obj$rules[i]
    }
    obj$._options <- .PKGOPT
  }
  # do options check.
}

# note: for some reason this function is not testable from devtools::test('pkg')
check_primitives <- function(){
  # todo: extract this from voptions()
   prim <- c("<","<=","==","!=",">=",">","%in%")
   for ( p in prim )
     if (!identical(body(p),body(getFromNamespace(p,"base"))))
       warning(sprintf("Using implementation of %s that differs from base::`%s`",p,p))
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
  calls <- .get_exprs(x,lin_eq_eps=0)
    
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


#' Combine two validator objects
#'
#' Combine two \code{\link{validator}} objects by addition. A new \code{validator} 
#' object is created with default (global) option values. Previously set options
#' are ignored. 
#'
#' @param e1 a \code{\link{validator}}
#' @param e2 a \code{\link{validator}}
#'
#' @section Note:
#' The \code{names} of the resulting object are made unique using \code{\link[base]{make.names}}.
#'
#'
#' @examples
#' validator(x>0) + validator(x<=1)
#'
#' @export
setMethod("+", c("validator","validator"), function(e1, e2){
  v <- validator()
  v$rules <- c(e1$rules, e2$rules)
  names(v) <- make.names(names(v),unique=TRUE)
  v
})





