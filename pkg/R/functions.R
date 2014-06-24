#' Syntax to define validation or indicator rules
#'
#' The functions mentioned in this help file should only be used in the
#' context of defining a \code{\link{validator}} or \code{\link{indicator}} object.
#'
#' @name syntax
#' 
#'
#' @section Local assignment:
#' The operator `\code{:=}' can be used to set up local variables (during, for example, validation) to save
#' time (the rhs of an assignment is computed only once) or to make your validation code more maintainable. 
#' Assignments work more or less like common R assignments: they are only valid for statements coming after 
#' the assignment and they may be overwritten. The result of computing the rhs is not part of a 
#' \code{\link{confront}}ation with data.
#' 
#' @section Predefined indicators:
#' 
#' \itemize{
#'    \item \code{number_missing}
#'    \item \code{fraction_missing}
#' }
#'   
#' @section Groups:
#' Often the same constraints/rules are valid for groups of variables. 
#' \code{validate} allows for compact notation. First define a group and than use
#' that group in a validator statement. 
#' \code{
#'   validator( g:{a;b}
#'            , g > 0
#'            )
#' }
#' 
#' Using two groups results in the cartesian product of checks.
#' \code{
#'   validator( f:{e;d}
#'            , g:{a;b}
#'            , g > f    # equals {a>e, a>d, b>e, b>d}
#'            )
#' }
NULL

# NOTE: the '*_missing' functions could probably be speeded up by writing dedicated C-implementations.


#' @param ... comma-separated list of variable names (not character) or a quoted regular expression. If no
#'  variables are specified, the number of missings over all data is counted.
#'  
#' @return For \code{number_missing}, the total number of missings over all specified variables.
#' @rdname syntax 
number_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
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
  vars <- matchvars(L,parent.frame())
  v <- sapply(
    eapply(
      env=parent.frame()
      , FUN = function(x) c(sum(is.na(x)),length(x))
    )[vars]
    ,Id)
  sum(v[1,])/sum(v[2,])
}

#' @rdname syntax
#' @return For \code{row_missing} a vector with the number of missings per (sub)record defined by \code{...}.
row_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
  rowSums(sapply(eapply(
    env=parent.frame()
    , FUN = is.na
    )[vars]
    ,Id))
}

#' @rdname syntax
#' @return For \code{col_missing} a vector with the number of missings per column defined by \code{...}.
col_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
  colSums(sapply(eapply(
    env=parent.frame()
    , FUN = is.na
  )[vars]
  ,Id))  
}



# returns a character vector of variables specified in L, matched in env.
matchvars <- function(L,env){
  if( length(L) == 0 ){
    TRUE 
  } else { 
    if (is.character(L[[1]])) {
      grep(pattern = L[[1]], x = ls(env), value = TRUE)  
    } else { 
      sapply(L,as.character)
    }
  }
}

#' @param rule R expression: a validation rule. Must result in a logical.
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
#' @param p $L^p$-norm to use (default is the Euclidean norm)
#' @return For \code{L}, a \code{list} containing the validator value, the impact function and the severity function
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
