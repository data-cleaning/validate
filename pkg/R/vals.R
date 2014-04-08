#' @include verifier.R
# implementation of the VALS syntax
# VALS version: 0.13.09


# Design idea:
# - vals object (child of verifier) containing 'calls' in VALS syntax.
# - confront method for VALS object translates VALS -> R and evaluates.
# - valsValue object containing results

#' Define validators in the VALS syntax
#'
#' @param ... A comma-separated list of VALS expressions (possibly named)
#' @param files \code{character}: file location(s) with VALS syntax
#'
#' @export
vals <- function(...,files=NULL) new('vals',...,files=files)

# vals object. VALS syntax consists of a (possible) number of assignmetns, followed 
# by one or more calls to 'validate' 
setRefClass('vals', contains= 'verifier'
  , methods = list(
    initialize = function(...,files=NULL) 
      .verifier(.self,...,files=files,prefix='VALS')
    )
)

# Somehow, the := operator is recognized by R's parser although `:=` is not exposed to the public.
# We define it here for our package.
`:=` <- function(x,y) assign(x=deparse(substitute(x)),value=y,envir=sys.frame(-1))

#' VALS validate function
#' 
#' This function is not to be called directly from R since it will be interpreted as VALS syntax.
#' 
#' @param bde BooleanDatasetexpression
#' @param discrepancy An optional numeric measure which is the result of calculation of a 
#'   discrepancy formula, if it is provided. Default discrepancy value is Null.
#' @param severity An optional numeric measure showing severity of validation error 
#'  for records that fail validation. By default, valid records are assigned severity equal 
#'  to zero, and invalid ones, equal to one. This facilitates calculations based on linear 
#'  combination of results.
#' 
#' @export 
validate <- function(bde
  , discrepancy=NULL
  , severity=NULL
  , outputType=c('is_valid','is_invalid')
  , level=c('record-level','dataset-level')){
  call <- substitute(bde)
  eval(expr=call,envir=parent.frame())
  
}

is_vals_assignment <- function(x) x[[1]] == ":="

setMethod('confront', signature('vals','data'), 
  function(x,y,...){
    e <- list2env(y,parent=parent.frame())
    i <- !sapply(x$calls,is_vals_assignment)
    lapply(x$calls, factory(eval),envir=e)[i]
})









