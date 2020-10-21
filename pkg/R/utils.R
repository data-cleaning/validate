
setNames <- function(x,nm){
  names(x) <- nm
  x
}

# a reasonable warning
warnf <- function(fmt,...){
  warning(sprintf(fmt,...),call.=FALSE)
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
#' rules <- validator(height >= 60,weight < 159)
#' satisfying(women, rules)
#' violating(women, rules)
#'
#' out <- confront(women, rules)
#' summary(out)
#' satisfying(women, out)
#' violating(women, out)
#'
#' @family select-data
#' @export
satisfying <- function(x, y, include_missing=FALSE, ...){
  stopifnot(inherits(y,"validator") | inherits(y,"validation"), inherits(x,"data.frame"))

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

#' @rdname satisfying
#' @export
violating <- function(x, y, include_missing=FALSE, ...){
  stopifnot(inherits(y,"validator") | inherits(y,"validation"), inherits(x,"data.frame"))

  if (inherits(y,"validator")) y <- confront(x,y,...)

  A <- values(y)
  if (!is.array(A)|| nrow(A)!=nrow(x) ){
    stop("Not all rules have record-wise output")
  }
  if (include_missing){
    x[apply(A,1, function(d) any(!d | is.na(d)) )   ] 
  } else {
    x[apply(A,1,function(d) any(!d &!is.na(d))),,drop=FALSE]
  }
  
}

#' @rdname satisfying
#' @export
lacking <- function(x, y, ...){
  stopifnot(inherits(y,"validator") | inherits(y,"validation"), inherits(x,"data.frame"))

  if (inherits(y,"validator")) y <- confront(x,y,...)

  A <- values(y)
  if (!is.array(A)|| nrow(A)!=nrow(x) ){
    stop("Not all rules have record-wise output")
  }
  x[apply(A,1,anyNA),,drop=FALSE]
}
