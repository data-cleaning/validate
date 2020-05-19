
#' Check whether a variable represents a linear sequence
#'
#' A variable \eqn{X = (x_1, x_2,\ldots, x_n)} (\eqn{n\geq 0}) represents a
#' \emph{linear sequence} when \eqn{x_{j+1} - x_j} is constant for all
#' \eqn{j\geq 1}. That is,  elements in the series are equidistant and without
#' gaps. 
#'
#' @details
#' 
#' Presence of a missing value (\code{NA}) in \code{x} will result in \code{NA},
#' except when \code{length(x) <= 2} and \code{start} and \code{end} are
#' \code{NULL}. Any sequence of length \eqn{\leq 2} is a linear sequence. 
#' 
#'
#'
#' @param x An R vector.
#' @param sort \code{[logical]}. When set to \code{TRUE}, \code{x}
#'        is sorted within each group before testing.
#' @param start Optionally, a value that should equal \code{min(x)}
#' @param end   Optionally, a value that should equal \code{max(x)}
#' @param ... bare (unquoted) variable names used to split \code{x}
#'        into groups. The check is executed for each group.
#'
#' @return For \code{is_linear_sequence}: a single \code{TRUE} or \code{FALSE},
#' equal to \code{all(in_linear_sequence)}.
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
#' ## check groupwise data
#' dat <- data.frame(
#'    time = c(2012, 2013, 2012, 2013, 2015)
#'  , type = c("hi", "hi", "ha", "ha", "ha")
#' )
#' rule <- validator(in_linear_sequence(time, type))
#' values(confront(dat, rule)) ## 2xT, 3xF
#'
#'
#' rule <- validator(in_linear_sequence(time, type))
#' values( confront(dat, rule) )
#'
#' @family long-form
#'
#' @export
is_linear_sequence <- function(x, ...) UseMethod("is_linear_sequence")

# workhorse function
is_lin_num_seq <- function(x, start=NULL, end=NULL, sort=TRUE, tol=1e-8){

  # Edge cases: empty sequence, or length 1 sequence with missing value.  In
  # those cases, return FALSE when any of start or end is checked, otherwise
  # return TRUE
  if ( length(x) <= 2 && all(is.na(x)) ) 
    return(is.null(start) && is.null(end))

  if (anyNA(x)) return(NA)

  # the regular case
  !anyNA(x) &&
    (is.null(start) || abs(start - min(x)) <= tol) &&
    (is.null(end)   || abs(end - max(x))   <= tol) &&
    ( length(x) <= 1 || { if(sort) x <- sort(x)
                          d <- diff(x)
                          all(abs(d - d[1]) <= tol)
                        })
}

as_int <- function(x){
  if( is.null(x)) NULL else as.integer(x)
}
as_num <- function(x){
  if (is.null(x)) NULL else as.numeric(x)
}


all_lin_num_seq <- function(x, ..., start=NULL, end=NULL, sort=TRUE, tol=1e-8){
  d <- list(...)
  if (length(d) == 0){
    is_lin_num_seq(x, start=start, end=end, sort=sort, tol=tol)
  } else {
    all(tapply(x, INDEX=d, FUN=is_lin_num_seq, start=start, end=end, sort=sort, tol=tol))
  }
} 



#' @rdname is_linear_sequence
#' @param tol numerical tolerance for gaps.
#' @export
is_linear_sequence.numeric <- function(x, ..., start=NULL, end=NULL, sort=TRUE, tol = 1e-8){
  all_lin_num_seq(x, ..., start=start, end=end, sort=sort, tol=1e-8)
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.Date <- function(x, ..., start=NULL, end=NULL, sort=TRUE){
  all_lin_num_seq(as.integer(x), start=as_int(start), end=as_int(end), sort=sort, tol=0)
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.POSIXct <- function(x, ... , start=NULL, end=NULL, sort = TRUE, tol=1e-6){
  # Note. POSIXct can express fractions of a second. Conversion from and to POSIXlt
  # is better than microseconds, so that is what we use as default tolerance/
  all_lin_num_seq(as.numeric(x), start=as_num(start), end=as_num(end), sort=sort, tol=tol)
}

#' @rdname is_linear_sequence
#'
#' @param format \code{[character]}. How to interpret \code{x} as a period.
#' Either \code{"auto"} for automatic detection or a specification passed to
#' \code{\link{strptime}}. Automatically detected periods are of the form year:
#' \code{"2020"}, yearMmonth: \code{"2020M01"},  yearQquarter: \code{"2020Q3"},
#' or year-Qquarter: \code{"2020-Q3"}. 
#'
#' @export
is_linear_sequence.character <- function(x, ..., start=NULL, end=NULL, sort=TRUE, format="auto"){
  if ( format == "auto" ){
    pt <- period_type(x)
    y     <- period_to_int(x, from=pt)
    start <- period_to_int(start, from = pt)
    end   <- period_to_int(end, from = pt)
    is_linear_sequence.numeric(y, ..., start=start, end=end, sort=sort, tol=0)
  } else {
    y     <- strptime(x, format=format)
    start <- strptime(start, format=format)
    end   <- strptime(end, format=format)
    is_linear_sequence.POSIXct(y, ..., start=start, end=end, sort=sort, tol=1e-6)
  }

}





#' @rdname is_linear_sequence
#'
#' @return For \code{in_linear_sequence}: a \code{logical} vector with the same length as \code{x}.
#' @export
in_linear_sequence <- function(x, ...) UseMethod("in_linear_sequence")

in_lin_num_seq <- function(x, start=NULL, end=NULL, sort=TRUE, tol=1e8){
  rep(is_lin_num_seq(x, start=start, end=end, sort=sort, tol=tol), length(x))
}


#' @rdname is_linear_sequence
#' @export
in_linear_sequence.numeric <- function(x, ..., start=NULL, end=NULL, sort=TRUE, tol=1e-8){
  d <- list(...)
  if (length(d) == 0){
    in_lin_num_seq(as.integer(x), start=as_int(start), end=as_int(end), sort=sort, tol=tol)
  } else {
    result <- tapply(as.integer(x), d, in_lin_num_seq, start=as_int(start), end=as_int(end), sort=sort, tol=tol)
    unsplit(result, d)
  }
}

#' @rdname is_linear_sequence
#' @export
in_linear_sequence.Date <- function(x, ..., start=NULL, end=NULL, sort=TRUE){
  in_linear_sequence.numeric(as.integer(x), ..., start=as_int(start), end=as_int(end), sort=TRUE, tol=0)
}


#' @rdname is_linear_sequence
#' @export
in_linear_sequence.POSIXct <- function(x, ..., start=NULL, end=NULL, sort=TRUE, tol=1e-6){
  in_linear_sequence.numeric(as.numeric(x), ..., start=as_num(start), end=as_num(end), sort=sort, tol=0)
}



period_type <- function(x, undefined=NA_character_){
  if ( all( grepl("^[12][0-9]{3}$",x) ) )           return("annual")
  if ( all( grepl("^[12][0-9]{3}-?Q[1-4]$",x) ) )   return("quarterly")
  if ( all( grepl("^[12][0-9]{3}M[01][0-9]$",x) ) ) return("monthly")

  warning("Undefined period type or different period types in single column.", call.=FALSE)
  undefined
}


#' Turn a period into an integer
#' 
#' Annual periods are turned in to the integer year. Quarterly 
#' and Monthly periods are turned in to the month number, counted
#' from the year zero, so quarters and months have consecutive numbers
#' accross years.
#'
#' @param x a \code{character} vector.
#' @param from \code{character} scalar, indicating the period format 
#' (see \code{\link{RTS}} for supported formats).
#'
#'
#' @examples
#' 
#' periods <- c("2018-Q4","2019-Q1")
#' period_to_int(periods, from="quarterly")
#'
period_to_int <- function(x, from = c("annual","quarterly","monthly")){
  from <- match.arg(from)
  if (is.null(x)) return(NULL)

  if (from == "annual"){
    res <- as.numeric(x)
  }


  if (from ==  "quarterly" ){
    L       <- strsplit(x,"-?Q")
    year    <- as.numeric(sapply(L, `[[`,1))
    quarter <- as.numeric(sapply(L, `[[`, 2))
    res     <- 4*year + quarter-1
  }

  if ( from == "monthly" ){
    L     <- strsplit(x, "M")
    year  <- as.numeric( sapply(L,`[[`,1) )
    month <- as.numeric( sapply(L, `[[`, 2) )
    res   <- 12*year + month-1 == 1
  }

  res
}




