#' @include expressionset.R
NULL



# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.

#' Define indicators for data
#' 
#' \bold{This feature is currently experimental and may change in future versions}
#' 
#' \code{\link{indicator}}
#' @param ... A comma-separated list of indicator definitions
#' @param .file (optional) A character vector of file locations
#' 
#' @seealso \code{\link{syntax}}
#' 
#' @export
#' @keywords internal
#' @example ../examples/indicator.R
indicator <- function(..., .file, .data) new('indicator',..., .file=.file, .data=.data)




#### INDICATOR CLASS ----------------------------------------------------------

#' Store a set of rich indicator expressions
#'
#' \bold{This feature is currently experimental and may change in future versions}
#' 
#' @section Details:
#' An indicator stores a set of indicators. It is a child class of \code{\link{expressionset}} and
#' can be constructed with  \code{\link{indicator}}.
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
setRefClass("indicator", contains='expressionset',
  methods = list(
    initialize = function(..., .file, .data) ini_indicator(.self
      , ..., .file = .file, .data=.data)
  )                       
)

ini_indicator <- function(obj, ..., .file, .data){
  
  if (missing(.file) && missing(.data)){
    .ini_expressionset_cli(obj, ..., .prefix="I")
    obj$._options <- .PKGOPT
  } else if (!missing(.file)) {
    .ini_expressionset_yml(obj, .file, .prefix="I")
  } else if (!missing(.data)){
    .ini_expressionset_df(obj, dat=.data, .prefix="I")
  }
}





