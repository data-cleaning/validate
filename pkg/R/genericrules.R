
#' Check whether a vector contains a linear sequence
#'
#' A vector of data points \eqn{x_1, x_2,\ldots, x_n} (\eqn{n\geq 0}) is a
#' \emph{linear sequence} when \eqn{x_{j+1} - x_j} is constant for all
#' \eqn{j\geq 1}. In particular, this implies that the elements in the series
#' are equidistant, without gaps. 
#'
#' @details
#' 
#' Any missing value (\code{NA}) in \code{x} will result in \code{FALSE},
#' except when \code{length(x) == 1} and \code{start} and \code{end} are
#' \code{NULL}. (Any sequence of length 1 is a linear sequence). 
#' 
#'
#'
#' @param x An R vector.
#' @param sort \code{[logical]}. When set to \code{TRUE}, \code{x}
#'        is sorted before testing.
#' @param start Optionally, a value that should equal \code{min(x)}
#' @param end   Optionally, a value that should equal \code{max(x)}
#' @param ... arguments passed to other methods.
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#'
#' is_linear_sequence(1:5) # TRUE
#' is_linear_sequence(c(1,3,5,4,2)) # FALSE
#' is_linear_sequence(c(1,3,5,4,2), sort=TRUE) # TRUE 
#' is_linear_sequence(NA_integer_) # TRUE
#' is_linear_sequence(NA_integer_, start=4) # FALSE
#' is_linear_sequence(c(1, NA, 3)) # FALSE
#'
#'
#' d <- data.frame(
#'     number = c(pi, exp(1), 7)
#'   , date = as.Date(c("2015-12-17","2015-12-19","2015-12-21"))
#'   , time = as.POSIXct(c("2015-12-17","2015-12-19","2015-12-20"))
#' )
#'
#' rules <- validator(
#'     is_linear_sequence(number)  # fails
#'   , is_linear_sequence(date)    # passes
#'   , is_linear_sequence(time)    # fails
#' )
#' summary(confront(d,rules))
#'
#' @export
is_linear_sequence <- function(x, ...) UseMethod("is_linear_sequence")

#' @rdname is_linear_sequence
#' @param tol numerical tolerance for gaps.
#' @export
is_linear_sequence.numeric <- function(x, start=NULL, end=NULL, sort=FALSE, tol = 1e-8, ...){

  # TODO edge case where all diffs are 0

  # Edge cases: empty sequence, or length 1 sequence with missing value.  In
  # those cases, return FALSE when any of start or end is checked, otherwise
  # return TRUE
  if ( length(x) == 0 || (length(x) == 1 && is.na(x)) ) 
    return(is.null(start) && is.null(end))

  # the regular case
  !anyNA(x) &&
    (is.null(start) || abs(start - min(x)) <= tol) &&
    (is.null(end)   || abs(end - max(x))   <= tol) &&
    ( length(x) <= 1 || { if(sort) x <- sort(x)
                          d <- diff(x)
                          all(abs(d - d[1]) <= tol)
                        })

}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.Date <- function(x,start=NULL, end=NULL, sort = FALSE, ...){
  if (sort) x <- sort(x)
  length(x) <= 1 || {d <- diff(x); all(d == d[1])}
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.POSIXct <- function(x, start=NULL, end=NULL, sort = FALSE, ...){
  is_linear_sequence.Date(x = x, start=start, end=end, sort = sort, ...)
}


