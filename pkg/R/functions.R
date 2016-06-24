#' Syntax to define validation or indicator rules
#'
#' The functions mentioned in this help file should only be used in the
#' context of defining a \code{\link{validator}} or \code{\link{indicator}} object.
#'
#' @section Note:
#' This document only provides a short reference. Please refer to the vignette for worked
#' examples.
#' 
#' vignette("intro",package="validate")
#'
#' @name syntax
#'
#' 
#' @section Refernce the dataset as a whole
#' 
#' Metadata such as numer of rows, columns, column names and so on can be 
#' tested by referencing the whole data set with the '\code{.}'. For example,
#' the rule \code{nrow(.) == 15} checks whether there are 15 rows in the
#' dataset at hand.
#'    
#'
#' @section Local, transient assignment:
#' The operator `\code{:=}' can be used to set up local variables (during, for example, validation) to save
#' time (the rhs of an assignment is computed only once) or to make your validation code more maintainable. 
#' Assignments work more or less like common R assignments: they are only valid for statements coming after 
#' the assignment and they may be overwritten. The result of computing the rhs is not part of a 
#' \code{\link{confront}}ation with data.
#'   
#'   
#' @section Groups:
#' Often the same constraints/rules are valid for groups of variables. 
#' \code{validate} allows for compact notation. Variable groups can be used in-statement
#' or by defining them with the \code{:=} operator.
#' 
#' \code{validator( var_group(a,b) > 0 )}
#' 
#' is equivalent to
#' 
#' \code{validator(G := var_group(a,b), G > 0)}
#' 
#' is equivalent to
#' 
#' \code{validator(a>0,b>0)}.
#' 
#' Using two groups results in the cartesian product of checks. So the statement
#'
#' \code{validator( f=var_group(c,d), g=var_group(a,b), g > f)}
#' 
#' is equivalent to
#' 
#' \code{validator(a > c, b > c, a > d, b > d)}
#' 
#' @section File parsing:
#' Please see the vignette on how to read rules from and write rules to file:
#' 
#' \code{vignette("rule-files",package="validate")}
#' 
#'   
NULL


#### MISSINGNES COUNTERS ------------------------------------------------------

# NOTE: the '*_missing' functions could probably be speeded up by writing dedicated C-implementations.

#' @param ... comma-separated list of variable names (not character). If no
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
#' @return For \code{col_missing} a vector with the number of missings per column 
#'    defined by \code{...}.
col_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
  colSums(sapply(eapply(
    env=parent.frame()
    , FUN = is.na
  )[vars]
  ,Id))  
}

#' @rdname syntax
#' @return For \code{number_unique} the number of records, unique for 
#'   the columns specified in \code{...}.
number_unique <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
  if ( identical(vars,TRUE)) vars <- ls(parent.frame())
  length(unique(do.call(paste0,mget(vars,parent.frame()))))
}

#' @rdname syntax
#' @return For \code{any_missing}, \code{TRUE} if any \code{NA} occur in the columns
#'   specified in \code{...}.
any_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
  e <- parent.frame()
  a <- FALSE
  if (identical(vars,TRUE)) vars <- ls(e)
  for ( v in vars ) a <- a | anyNA(e[[v]])
  a
}

#' @rdname syntax
#' @return For \code{any_duplicated}, \code{TRUE} if any (sub)records specified by
#'  \code{...} are duplicated, \code{FALSE} otherwise. Note that \code{NA} is matched with \code{NA}.
any_duplicated <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- matchvars(L,parent.frame())
  if (identical(vars,TRUE)) vars <- ls(parent.frame())
  anyDuplicated( do.call(paste0,mget(vars,parent.frame())) ) > 0
}

# variational functions act on a chosen set of variables. If no variables are
# chosen they act on all variables. This function detect if such a funtion is
# present in a call.

VARFUN <- c(
  "number_missing"
  , "fraction_missing" 
  , "row_missing"
  , "col_missing"
  , "number_unique"
  , "any_missing"
  , "any_duplicated"
)




#### FUNCTIONAL DEPENDENCIES --------------------------------------------------

# Internal function that tests for functional dependencies
`~` <- function(lhs, rhs){
  Lvars <- all.vars(substitute(lhs))
  Rvars <- all.vars(substitute(rhs))
  
  condition  <- do.call(paste, c(mget(Lvars, parent.frame()), sep="|"))
  consequent <- do.call(paste0, c(mget(Rvars, parent.frame()), sep="|"))
  cf <- .Call("R_fdcheck", condition, consequent)
  cf == seq_along(cf)
}

# synonym of `~`, may be more understandable
`%->%` <- `~`


# returns a character vector of variables specified in L, matched in env.
# regexps are switched off untill we can analyze relation with literal variables better.
matchvars <- function(L,env){
  if( length(L) == 0 ){
    TRUE 
  } else { 
#    if (is.character(L[[1]])) {
#      grep(pattern = L[[1]], x = ls(env), value = TRUE)  
#    } else { 
      sapply(L,as.character)
#    }
  }
}

# @param rule R expression: a validation rule. Must result in a logical.
# @param impact R expression: an expression. Must result in a numeric.
# @param severity R expression: an expression. Must result in a numeric.
# @rdname syntax
# @return For \code{V} a \code{list} containing the return values of \code{rule}, \code{impact} and \code{severity}
# V <- function(rule, impact=NULL, severity=NULL){
#   r <- substitute(rule)
#   i <- substitute(impact)
#   s <- substitute(severity)
#   list(
#     result   = eval(r,envir=sys.parent())
#     , impact   = eval(i,envir=sys.parent())
#     , severity = eval(s,envir=sys.parent())
#   )
# }

# severity and impact for Linear validators

# @rdname syntax
# @param linrule A \emph{linear} validating expression
# @param p $L^p$-norm to use (default is the Euclidean norm)
# @return For \code{L}, a \code{list} containing the validator value, the impact function and the severity function
# L <- function(linrule, p=2){
#   e <- substitute(linrule)
#   q <- p/(p-1)
#   a <- const_norm(e,p/(p-1))
#   result <- eval(e,envir=sys.parent())
#   severity <- abs(eval(left(e),envir=sys.parent()) - eval(right(e),envir=sys.parent()))  
#   impact <- severity/a
#   list(result=result,severity=severity,impact=impact)
# }

# const_norm <- function(expr,q){
#   l <- coefficients(left(expr))
#   r <- coefficients(right(expr))
#   vars <- unique(names(c(l,r)))
#   a <- setNames(numeric(length(vars)),vars)
#   a[names(l)] <- l
#   a[names(r)] <- a[names(r)] - r
#   a = sum(abs(a[!names(a)=='CONSTANT'])^q)^(1/q)
# }

# d <- data.frame(
#   x = c(1,NA,3,5)
#   , y = c(NA,NA,1,2)
#   , z = letters[1:4]
#   )
# I <- indicator(fraction_missing(x),number_missing(),fraction_missing(x,z))
# 
# values(confront(I,d))
