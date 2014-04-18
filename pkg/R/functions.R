
#' Syntax to define validation rules
#'
#' The functions mentioned in this help file should only be used in the
#' context of defining a \code{\link{validator}} or \code{\link{indicator}} object.
#'
#' @name syntax
#'  
#'
NULL


# functions, added to the syntax of validator and indicator objects



#' @param ... comma-separated list of variable names (not character). If no
#'  variables are specified, the number of missings over all data is counted.
#'  
#' @return For \code{number_missing}, the total number of missings over all specified variables.
#' @rdname syntax 
#' @export
number_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- if( length(L) == 0 ) TRUE else sapply(L,as.character)
  sum(sapply(
    eapply(
      env=parent.frame()
      , FUN = function(x) sum(is.na(x)) 
    )[vars]
  ,Id))
}

#' @rdname syntax
#' @return For \code{fraction_missing}, the fraction of missings over all specified variables
fraction_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- if( length(L) == 0 ) TRUE else sapply(L,as.character)
  v <- sapply(
    eapply(
      env=parent.frame()
      , FUN = function(x) c(sum(is.na(x)),length(x))
    )[vars]
    ,Id)
  sum(v[1,])/sum(v[2,])
}


#' @param rule R expression: a validation rule. Must result in a (vector of) logical(s).
#' @param impact R expression: an expression. Must result in a numeric.
#' @param severity R expression: an expression. Must result in a numeric.
#' @rdname syntax
#' @return For \code{V} a \code{list} containing the return values of \code{rule}, \code{impact} and \code{severity}
V <- function(rule, impact=NULL, severity=NULL){
  r <- substitute(rule)
  i <- substitute(impact)
  s <- substitute(severity)
  list(
    result   = eval(r,envir=sys.parent())
    , impact   = eval(i,envir=sys.parent())
    , severity = eval(s,envir=sys.parent())
  )
}

# severity and impact for Linear validators

#' @rdname syntax
#' @param linrule A \emph{linear} validating expression
#' @param p Lp-norm to use (default is the Euclidean norm)
#' @return For \code{L}, a \code{list} containing the validator value, the impact function and the severity function
#' @export 
L <- function(linrule, p=2){
  e <- substitute(linrule)
  q <- p/(p-1)
  a <- const_norm(e,p/(p-1))
  result <- eval(e,envir=sys.parent())
  severity <- abs(eval(left(e),envir=sys.parent()) - eval(right(e),envir=sys.parent()))  
  impact <- severity/a
  list(result=result,severity=severity,impact=impact)
}

const_norm <- function(expr,q){
  l <- coefficients(left(expr))
  r <- coefficients(right(expr))
  vars <- unique(names(c(l,r)))
  a <- setNames(numeric(length(vars)),vars)
  a[names(l)] <- l
  a[names(r)] <- a[names(r)] - r
  a = sum(abs(a[!names(a)=='CONSTANT'])^q)^(1/q)
}

# d <- data.frame(
#   x = c(1,NA,3,5)
#   , y = c(NA,NA,1,2)
#   , z = letters[1:4]
#   )
# I <- indicator(fraction_missing(x),number_missing(),fraction_missing(x,z))
# 
# values(confront(I,d))
