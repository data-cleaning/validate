#' @include expressionset.R
NULL

#   library(settings)
#   source('pkg/R/sugar.R')
#   source('pkg/R/parse.R')
#   source('pkg/R/rule.R')
#   source('pkg/R/expressionset.R')


# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.

#' Define indicators for data
#' 
#' \code{\link{indicator}}
#' @param ... A comma-separated list of indicator definitions
#' @param .file (optional) A character vector of file locations
#' 
#' @seealso \code{\link{syntax}}
#' 
#' @export
#' @example ../examples/indicator.R
indicator <- function(..., .file) new('indicator',..., .file=.file)

setRefClass("indicator", contains='expressionset',
  methods = list(
    initialize = function(..., .file) ini_indicator(.self, ..., .file = .file)
  )                       
)

ini_indicator <- function(obj, ..., .file){
  
  if (missing(.file)){
    ini_expressionset_cli(obj, ..., .prefix="I")
    obj$._options <- PKGOPT
  } else {
    ini_expressionset_yml(obj, file, .prefix="V")
  }
  obj
}





