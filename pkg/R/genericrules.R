
#' Check whether a vector contains a strictly monotone sequence of data
#'
#' A series of data points \eqn{x_1, x_2,\ldots} is a strictly monotone
#' sequence when \eqn{x_{j+1} - x_j} is constant unequal to zero for all
#' \eqn{j\geq 0}. In particular, this implies that the series has no gaps and
#' no duplicates.
#'
#' @param x An R vector.
#' @param sort \code{[logical]} toggle sort before test
#' @param ... arguments passed to other methods.
#'
#' @examples
#'
#' is_sequence(1:5) # TRUE
#' is_sequence(c(1,3,5,4,2)) # TRUE
#' is_sequence(c(1,3,5,4,2), sort=FALSE) # FALSE 
#'
#' d <- data.frame(
#'     number = c(pi, exp(1), 7)
#'   , date = as.Date(c("2015-12-17","2015-12-19","2015-12-21"))
#'   , time = as.POSIXct(c("2015-12-17","2015-12-19","2015-12-20"))
#' )
#'
#' rules <- validator(
#'     is_sequence(number)  # fails
#'   , is_sequence(date)    # passes
#'   , is_sequence(time)    # fails
#' )
#' summary(confront(d,rules))
#'
#' @export
is_sequence <- function(x, sort = TRUE, ...) UseMethod("is_sequence")

#' @rdname is_sequence
#' @param tol numerical tolerance for gaps.
#' @export
is_sequence.numeric <- function(x, sort=TRUE, tol = 1e-8, ...){
  if (sort) x <- sort(x)
  length(x) <= 1 || {d <- diff(x); all(d - d[1] <= tol)}
}

#' @rdname is_sequence
#' @export
is_sequence.Date <- function(x, sort = TRUE, ...){
  if (sort) x <- sort(x)
  length(x) <= 1 || {d <- diff(x); all(d == d[1])}
}

#' @rdname is_sequence
#' @export
is_sequence.POSIXct <- function(x, sort = TRUE, ...){
  is_sequence.Date(x = x, sort = sort, ...)
}


