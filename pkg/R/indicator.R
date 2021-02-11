#' @include expressionset.R
NULL



# The 'indicator' class holds indicator definitions

#' Define indicators for data
#' 
#' An indicator maps a data frame, or each record in a data frame to a number.
#' The purpose of this class is to store and apply expressions that define
#' indicators.
#' 
#' 
#' @param ... A comma-separated list of indicator definitions
#' @param .file (optional) A character vector of file locations
#' 
#' @seealso \code{\link{syntax}}, \code{\link{add_indicators}}
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
#'  \itemize{
#'  \item{Methods inherited from \code{\link{expressionset}}}
#'    \item{\code{\link{confront}}}
#'    \item{\code{\link{compare}}}
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
    obj$._options <- .PKGOPT
  }
}


#' Combine two indicator objects
#'
#' Combine two \code{\link{indicator}} objects by addition. A new \code{indicator} 
#' object is created with default (global) option values. Previously set options
#' are ignored.
#'
#' @param e1 a \code{\link{validator}}
#' @param e2 a \code{\link{validator}}
#'
#'
#' @family indicator-methods
#' @examples
#' indicator(mean(x)) + indicator(x/median(x))
#' @export
setMethod("+", c("indicator","indicator"), function(e1, e2){
  ii <- indicator()
  ii$rules <- c(e1$rules, e2$rules)
  names(ii) <- make.names(names(ii),unique=TRUE)
  ii
})


#' Add indicator values as columns to a data frame
#'
#' Compute and add externally defined indicators to data frame.
#' If necessary, values are recycled over records.
#'
#' @param dat \code{[data.frame]}
#' @param x   \code{[indicator]} or \code{[indication]} object. See examples.
#'
#' @return \code{dat} with extra columns defined by \code{x} attached.
#'
#' @examples
#' ii <- indicator(
#'  hihi = 2*sqrt(height)
#'  , haha = log10(weight)
#'  , lulz = mean(height)
#'  , wo0t = median(weight)
#' )
#' 
#' # note: mean and median are repeated
#' add_indicators(women, ii)
#'
#' # compute indicators first, then add
#' out <- confront(women, ii)
#' add_indicators(women, out)
#'
#' @family indicators
#'
#' @export
add_indicators <- function(dat, x){
  if (inherits(x,"indicator")) x <- confront(dat, x)
  vals <- values(x, simplify=FALSE)
  n <- nrow(dat)
  L <- lapply(vals, function(d){
    if (length(d) == n){
      d
    } else if (length(d)==1) {
      rep(d,n)
    } else {
      warnf("Skipping output that does not fit in data frame")
      NULL
    }
  })
  cbind(dat, do.call("cbind", L))
}





