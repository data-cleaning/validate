
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
#' @param by bare (unquoted) variable name or a list of unquoted variable names, 
#'        used to split \code{x} into groups. The check is executed for each group.
#' @param ... Arguments passed to other methods.
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
#' rule <- validator(in_linear_sequence(time, by=type))
#' values(confront(dat, rule)) ## 2xT, 3xF
#'
#'
#' rule <- validator(in_linear_sequence(time, type))
#' values( confront(dat, rule) )
#'
#' @family cross-record-helpers
#'
#' @export
is_linear_sequence <- function(x, by=NULL,...) UseMethod("is_linear_sequence")

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


all_lin_num_seq <- function(x, by=NULL, start=NULL, end=NULL, sort=TRUE, tol=1e-8){
  
  if (length(by) == 0){
    is_lin_num_seq(x, start=start, end=end, sort=sort, tol=tol)
  } else {
    all(tapply(x, INDEX=by, FUN=is_lin_num_seq, start=start, end=end, sort=sort, tol=tol))
  }
} 



#' @rdname is_linear_sequence
#' @param tol numerical tolerance for gaps.
#' @export
is_linear_sequence.numeric <- function(x, by=NULL, start=NULL, end=NULL, sort=TRUE, tol = 1e-8,...){
  all_lin_num_seq(x, by=by, start=start, end=end, sort=sort, tol=1e-8)
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.Date <- function(x, by=NULL, start=NULL, end=NULL, sort=TRUE,...){
  all_lin_num_seq(as.integer(x), by=by, start=as_int(start), end=as_int(end), sort=sort, tol=0)
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.POSIXct <- function(x, by=NULL , start=NULL, end=NULL, sort = TRUE, tol=1e-6,...){
  # Note. POSIXct can express fractions of a second. Conversion from and to POSIXlt
  # is better than microseconds, so that is what we use as default tolerance/
  all_lin_num_seq(as.numeric(x), by=by, start=as_num(start), end=as_num(end), sort=sort, tol=tol)
}

#' @rdname is_linear_sequence
#'
#' @param format \code{[character]}. How to interpret \code{x} as a time period.
#' Either \code{"auto"} for automatic detection or a specification passed to
#' \code{\link{strptime}}. Automatically detected periods are of the form year:
#' \code{"2020"}, yearMmonth: \code{"2020M01"},  yearQquarter: \code{"2020Q3"},
#' or year-Qquarter: \code{"2020-Q3"}. 
#'
#' @export
is_linear_sequence.character <- function(x, by=NULL, start=NULL, end=NULL, sort=TRUE, format="auto",...){
  if ( format == "auto" ){
    pt <- period_type(x)
    y     <- period_to_int(x, from=pt)
    start <- period_to_int(start, from = pt)
    end   <- period_to_int(end, from = pt)
    is_linear_sequence.numeric(y, by=by, start=start, end=end, sort=sort, tol=0,...)
  } else {
    y     <- strptime(x, format=format)
    start <- strptime(start, format=format)
    end   <- strptime(end, format=format)
    is_linear_sequence.POSIXct(y, by=by, start=start, end=end, sort=sort, tol=1e-6,...)
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
#' 
#'
#'
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


#' Check variable range
#'
#' Test wether a variable falls within a range.
#'
#' @param x A bare (unquoted) variable name.
#' @param min lower bound
#' @param max upper bound
#' @param ... arguments passed to other methods
#'
#'
#' @examples
#'
#' d <- data.frame(
#'    number = c(3,-2,6)
#'  , time   = as.Date(c("2018-02-01", "2018-03-01", "2018-04-01"))
#'  , period = c("2020Q1", "2021Q2", "2020Q3") 
#' )
#'
#' rules <- validator(
#'    in_range(number, min=-2, max=7, strict=TRUE)
#'  , in_range(time,   min=as.Date("2017-01-01"), max=as.Date("2018-12-31"))
#'  , in_range(period, min="2020Q1", max="2020Q4")
#' )
#'
#' result <- confront(d, rules)
#' values(result)
#'
#'
#' @export
in_range <- function(x, min, max,...) UseMethod("in_range")

#' @rdname in_range
#' @param strict \code{[logical]} Toggle between including the range boundaries
#'               (default) or not including them (when strict=TRUE).
#' 
#' @export             
in_range.default <- function(x, min, max, strict=FALSE, ...){
  if (strict) x > min && x < max
  else x >= min && x <= max
}

#' @rdname in_range
#'
#' @param format \code{[character]} of \code{NULL}. If \code{format=NULL} the
#' character vector is interpreted as is. And the whether a character lies
#' within a character range is determined by the collation order set by the
#' current locale. See the details of "\code{\link{<}}".  If \code{format} is
#' not \code{NULL}, it specifies how to interpret the character vector as a
#' time period.  It can take the value \code{"auto"} for automatic detection or
#' a specification passed to \code{\link{strptime}}. Automatically detected
#' periods are of the form year: \code{"2020"}, yearMmonth: \code{"2020M01"},
#' yearQquarter: \code{"2020Q3"}, or year-Qquarter: \code{"2020-Q3"}. 
#'
#'
#' @export
in_range.character <- function(x, min, max, strict=FALSE, format = "auto",...){
  if (is.null(format)) 
    in_range.default(x=x, min=min, max=max, strict=strict, ...)
  else if ( format == "auto" ){
    pt    <- period_type(x)
    y     <- period_to_int(x, from=pt)
    ymin  <- period_to_int(min, from = pt)
    ymax  <- period_to_int(max, from = pt)
    in_range(y, min=ymin, max=ymax, strict=strict, ...)
  } else {
    y     <- strptime(x, format=format)
    ymin  <- strptime(min, format=format)
    ymax  <- strptime(max, format=format)
    in_range(y, min=ymin, max=ymax, strict=strict, ...)
  }
}

#' Test whether details combine to a chosen aggregate
#' 
#' Data in 'long' format often contain records representing totals
#' (or other aggregates) as well as records that contain details
#' that add up to the total. This function facilitates checking the
#' part-whole relation in such cases.
#'
#'
#' @param values A bare (unquoted) variable name holding the values to aggregate
#' @param labels A bare (unquoted) variable name holding the labels indicating
#'    whether a value is an aggregate or a detail.
#' @param whole \code{[character]} regex recognizing a whole in \code{labels}
#' @param part \code{[character]} regex recognizing a part in \code{labels}. If not specified,
#'        (\code{NULL}) all values not selected by \code{whole_pat} are used.
#' @param aggregator \code{[function]} used to aggregate subsets of \code{x}. It should
#'   accept a \code{numeric} vector and return a single number.
#' @param tol \code{[numeric]} tolerance for equality checking
#' @param by Name of variable, or \code{list} of bare variable names, used to
#'        split the values and labels before computing the aggregates. 
#' @param ... Extra arguments passed to aggregator (for example \code{na.rm=TRUE}).
#'  
#'
#' @return A \code{logical} vector of size \code{length(value)}.
#'
#' @examples
#' df <- data.frame(
#'    id = 10011:10020
#'  , period   = rep(c("2018Q1", "2018Q2", "2018Q3", "2018Q4","2018"),2)
#'  , direction = c(rep("import",5), rep("export", 5))
#'  , value     = c(1,2,3,4,10, 3,3,3,3,13)
#' )
#' rules <- validator(
#'   check_part_whole_relation(value, period, whole="^\\d{4}$", by=direction)
#' )
#'
#' out <- confront(df, rules, key="id")
#' as.data.frame(out)
#' @export
check_part_whole_relation <- function(values, labels, whole, part = NULL
    , aggregator = sum, tol=1e-8, by = NULL, ...){
  
  df <- data.frame(values=values, labels=labels)
  f <- function(d, ...){
    aggregate   <- d$values[grepl(whole, d$labels)]
    details     <- if (is.null(part)) d$values[!grepl(whole, d$labels)]
                   else  labels[grepl(part, d$labels)]
    if (length(aggregate)>1) stop("Multiple labels matching aggregate. Expecting one", call.=FALSE)
    out <- abs(aggregator(details, ...) - aggregate) < tol
    rep(out, length(d$labels))
  }

  
  if (length(by) < 1){
    return( f(df, ...) )
  } else {
    unsplit(lapply(split(df, by), f, ...),by)
  }

}


#' split-apply-combine for vectors, with equal-length outptu
#'
#' Group \code{x} by one or more categorical variables, compute
#' an aggregate, repeat that aggregate to match the size of the
#' group, and combine results. The functions \code{sum_by} and 
#' so on are convenience wrappers that call \code{do_by} internally.
#'
#' @param x A bare variable name
#' @param by a bare variable name, or a list of bare variable names, used to
#'        split \code{x} into groups.
#' @param fun \code{[function]} A function that aggregates \code{x} to a single value.
#' @param ... passed as extra arguments to \code{fun} (e.g. \code{na.rm=TRUE}
#' @param na.rm Toggle ignoring \code{NA}
#'
#' @examples
#' x <- 1:10
#' y <- rep(letters[1:2], 5)
#' do_by(x, by=y, fun=max)
#' do_by(x, by=y, fun=sum)
#'
#' @family cross-record-helpers
#' @export
do_by <- function(x, by, fun, ...){
  unsplit( lapply(split(x,by), function(d) rep(fun(d,...), length(d))),by)
}

#' @rdname do_by
#' @export
sum_by <- function(x, by, na.rm=FALSE) do_by(x,by,sum, na.rm=na.rm)

#' @rdname do_by
#' @export
mean_by <- function(x, by, na.rm=FALSE) do_by(x,by,mean, na.rm=na.rm)

#' @rdname do_by
#' @export
min_by <- function(x, by, na.rm=FALSE) do_by(x,by,min, na.rm=na.rm)

#' @rdname do_by
#' @export
max_by <- function(x, by, na.rm=FALSE) do_by(x,by,max, na.rm=na.rm)


#' Check number of code points
#'
#' A convenience function testing for field length.
#'
#'
#' @param x Bare (unquoted) name of a variable. 
#'     Otherwise a vector of class \code{character}. Coerced to character as 
#'     necessary.
#' @param n Number of code points required.
#' @param min Mimimum number of code points
#' @param max Maximum number of code points
#'
#' @section Details:
#'
#' The number of code points (string length) may depend on current locale
#' settings or encoding issues, including those caused by inconsistent choices
#' of \code{UTF} normalization.
#'
#' @return A \code{[logical]} of size \code{length(x)}.
#' 
#' @examples
#'
#' df <- data.frame(id = 11001:11003, year = c("2018","2019","2020"), value = 1:3)
#' rule <- validator(check_field_length(year, 4), check_field_length(id, 5))
#' out <- confront(df, rule) 
#' as.data.frame(out)
#'
#' @export
check_field_length <- function(x, n=NULL, min=NULL, max=NULL){
  len <- nchar(as.character(x))

  if (!is.null(n) & is.null(min) & is.null(max)){
    len == n 
  } else if (!is.null(min) & !is.null(max) & is.null(n) ){
    len >= min & len <= max
  } else {
    stop("Ill-specified check: either n, or min and max must be not-NULL")
  }
}

#' Check the layouts of numbers.
#'
#' Convenience function to check layout of numbers stored as
#' a character vector.
#'
#'
#' @param x \code{[character]} vector. If \code{x} is not of type
#'          \code{character} it will be converted.
#' @param format \code{[character]} denoting the number format (see below).
#'
#' @section Specifying numerical formats:
#' 
#' Numerical formats can be specified as a sequence of characters. There are a few 
#' special characters:
#' \itemize{
#'  \item{\code{d}} Stands for digit.
#'  \item{\code{*}} (digit globbing) zero or more digits
#' }
#' 
#' Here are some examples.
#' \tabular{ll}{
#' \code{"d.dd"}   \tab One digit, a decimal point followed by two digits.\cr
#' \code{"d.ddddddddEdd"}\tab Scientific notation with eight digits behind the decimal point.\cr
#' \code{"0.ddddddddEdd"}\tab Same, but starting with a zero.\cr
#' \code{"d,dd*"} \tab one digit before the comma and at least two behind it.\cr
#' }
#'
#'
#' @examples 
#' df <- data.frame(number = c("12.34","0.23E55","0.98765E12"))
#' rules <- validator(
#'    check_number_format(number, format="dd.dd")
#'    , check_number_format(number, "0.ddEdd")
#'    , check_number_format(number, "0.*Edd")
#' )
#'
#' out <- confront(df, rules)
#' values(out)
#'
#' @export
check_number_format <- function(x, format){
  rx <- utils::glob2rx(format)
  rx <- gsub("d","\\d", rx, fixed=TRUE)
  grepl(rx, as.character(x))
}






