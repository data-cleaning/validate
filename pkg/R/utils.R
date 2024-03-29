
setNames <- function(x,nm){
  names(x) <- nm
  x
}

# a reasonable warning and message
warnf <- function(fmt,...){
  warning(sprintf(fmt,...),call.=FALSE)
}

msgf <- function(fmt, ...){
  message(sprintf(fmt, ...))
}


#' Select records (not) satisfying rules
#' 
#' Apply validation rules or validation results to a data set and select only
#' those that satisfy all or violate at least one rule.
#'
#'
#' @param x A \code{data.frame}
#' @param y a \code{\link{validator}} object or a \code{\link{validation}} object.
#' @param include_missing Toggle: also select records that have \code{NA} output for a rule?
#' @param ... options passed to \code{\link{confront}}
#'
#' @note
#' An error is thrown if the rules or validation results in \code{y} can not be
#' interpreted record-by record (e.g. when one of the rules is of the form
#' \code{mean(foo)>0}).
#' 
#'
#' @return For \code{satisfying}, the records in \code{x} satisfying all rules or
#'         validation outcomes in \code{y}. For \code{violating} the records in
#'         \code{x} violating at least one of the rules or validation outcomes
#'         in \code{y}
#'
#' @examples
#' rules <- validator(speed >= 12, dist < 100)
#' satisfying(cars, rules)
#' violating(cars, rules)
#'
#' out <- confront(cars, rules)
#' summary(out)
#' satisfying(cars, out)
#' violating(cars, out)
#'
#' @family select-data
#' @export
satisfying <- function(x, y, include_missing=FALSE, ...){
  UseMethod("satisfying")
}

#' @export
satisfying.data.frame <- function(x, y, include_missing=FALSE, ...){
  stopifnot(inherits(y,"validator") | inherits(y,"validation"))

  if (inherits(y,"validator")) y <- confront(x,y,...)

  A <- values(y)
  if (!is.array(A)|| nrow(A)!=nrow(x) ){
    stop("Not all rules have record-wise output")
  }
  if (include_missing){
    x[apply(A,1,function(d) all(d | is.na(d)) ),,drop=FALSE]
  } else {
    x[apply(A,1,function(d) all(d &!is.na(d)) ), , drop=FALSE]
  }
}

#' @export
satisfying.default <- function(x,y,include_missing=FALSE, ...){
  stop("Not implemented for ", class(x), call. = FALSE)
}



#' @rdname satisfying
#' @export
violating <- function(x, y, include_missing=FALSE, ...){
  UseMethod("violating")
}

#' @export
violating.data.frame <- function(x, y, include_missing=FALSE, ...){
  stopifnot(inherits(y,"validator") | inherits(y,"validation"))

  if (inherits(y,"validator")) y <- confront(x,y,...)

  A <- values(y)
  if (!is.array(A)|| nrow(A)!=nrow(x) ){
    stop("Not all rules have record-wise output")
  }
  if (include_missing){
    x[apply(A, 1, function(d) any(!d | is.na(d))), , drop = FALSE] 
  } else {
    x[apply(A,1,function(d) any(!d &!is.na(d))),,drop=FALSE]
  }
  
}

#' @rdname satisfying
#' @export
violating.default <- function(x, y, include_missing=FALSE, ...){
  stop("Not implemented for ", class(x), call. = TRUE)
}

#' @rdname satisfying
#' @export
lacking <- function(x,y, ...){
  UseMethod("lacking")
}

#' @export
lacking.data.frame <- function(x, y, ...){
  stopifnot(inherits(y,"validator") | inherits(y,"validation"))

  if (inherits(y,"validator")) y <- confront(x,y,...)

  A <- values(y)
  if (!is.array(A)|| nrow(A)!=nrow(x) ){
    stop("Not all rules have record-wise output")
  }  
  x[apply(A,1,anyNA),,drop=FALSE]
}

#' @export
lacking.default <- function(x, y, ...){
  stop("Not implemented for ", class(x), call. = TRUE)
}
