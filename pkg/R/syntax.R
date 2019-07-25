#' @importFrom stats complete.cases
{}


#' Syntax to define validation or indicator rules
#'
#' A concise overview of the \code{validate} syntax.
#' 
#' @name syntax
#'
#' @section Basic syntax:
#' 
#' The basic rule is that an R-statement that evaluates to a \code{logical} is a
#' validating statement. This is established by static code inspection when
#' \code{validator} reads a (set of) user-defined validation rule(s).
#' 
#' @section Comparisons:
#' 
#' All basic comparisons, including \code{>, >=, ==, !=, <=, <}, \code{\%in\%}
#' are validating statements. When executing a validating statement, the
#' \code{\%in\%} operator is replaced with \code{\link[validate:vin]{\%vin\%}}.
#' 
#' @section Logical operations:
#' 
#' Unary logical operators `\code{!}', \code{all()} and \code{any} define
#' validating statements. Binary logical operations including \code{&, &&, |,
#' ||}, are validating when \code{P} and \code{Q} in e.g. \code{P & Q} are
#' validating. (note that the short-circuits \code{&&} and \code{&} onnly return
#' the first logical value, in cases where for \code{P && Q}, \code{P} and/or
#' \code{Q} are vectors. Binary logical implication \eqn{P\Rightarrow Q} (P
#' implies Q) is implemented as \code{if ( P ) Q}. The latter is interpreted as
#' \code{!(P) | Q}.
#' 
#' @section Type checking:
#' 
#' Any function starting with \code{is.} (e.g. \code{is.numeric}) is a
#' validating expression.
#' 
#' @section Text search:
#' 
#' \code{grepl} is a validating expression.
#' 
#' @section Functional dependencies:
#' 
#' Armstrong's functional dependencies, of the form \eqn{A + B \to C + D} are
#' represented using the \code{~}, e.g. \code{A + B ~ C + D}. For example
#' \code{postcode ~ city} means, that when two records have the same value for
#' \code{postcode}, they must have the same value for \code{city}.
#' 
#' 
#' @section Reference the dataset as a whole:
#' 
#' Metadata such as numer of rows, columns, column names and so on can be 
#' tested by referencing the whole data set with the '\code{.}'. For example,
#' the rule \code{nrow(.) == 15} checks whether there are 15 rows in the
#' dataset at hand.
#' 
#' @section Uniqueness, completeness:
#'
#' These can be tested in principle with the 'dot' syntax. However, there are
#' some convenience functions: \code{\link{is_complete}}, \code{\link{all_complete}}
#' \code{\link{is_unique}}, \code{\link{all_unique}}.
#'
#'
#' @section Local, transient assignment:
#' The operator `\code{:=}' can be used to set up local variables (during, for
#' example, validation) to save time (the rhs of an assignment is computed only
#' once) or to make your validation code more maintainable.  Assignments work more
#' or less like common R assignments: they are only valid for statements coming
#' after the assignment and they may be overwritten. The result of computing the
#' rhs is not part of a \code{\link{confront}}ation with data.
#'   
#'   
#' @section Groups:
#' Often the same constraints/rules are valid for groups of variables. 
#' \code{validate} allows for compact notation. Variable groups can be used
#' in-statement or by defining them with the \code{:=} operator.
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



### CONSISTENT SET MEMBERSHIP --------------------------------------------------

#' A consistent set membership operator
#' 
#' A set membership operator like \code{\link[base:match]{\%in\%}} that handles
#' \code{NA} more consistently with R's other logical comparison operators.
#'
#'
#' @details
#' R's basic comparison operators (almost) always return \code{NA} when one 
#' of the operands is \code{NA}. The \code{\%in\%} operator is an exception.
#' Compare for example \code{NA \%in\% NA} with \code{NA == NA}: the first
#' results in \code{TRUE}, while the latter results in \code{NA} as expected.
#' The \code{\%vin\%} operator acts consistent with operators such as \code{==}.
#' Specifically, \code{NA} results in the following cases.
#' \itemize{
#'  \item{For each position where \code{x} is \code{NA}, the result is \code{NA}.}
#'  \item{When \code{table} contains an \code{NA}, each non-matched value in 
#'  \code{x} results in \code{NA}.}
#' }
#'
#'
#'
#' @param x vector or \code{NULL}: the values to be matched
#' @param table vector or \code{NULL}: the values to be matched against.
#'
#' 
#' @examples 
#' # we cannot be sure about the first element:
#' c(NA, "a") %vin% c("a","b")
#' 
#' # we cannot be sure about the 2nd and 3rd element (but note that they
#' # cannot both be TRUE):
#' c("a","b","c") %vin% c("a",NA)
#' 
#' # we can be sure about all elements:
#' c("a","b") %in% character(0)
#' 
#' @rdname vin
#' @export
"%vin%" <- function(x, table){
  out <- match(x, table, nomatch=0) > 0
  if (anyNA(table)){
    out[!out] <- NA
  }
  out[is.na(x)] <- NA
  out
}




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


#### UNIQUENESS ---------------------------------------------------------------

#' Test for uniquenes of records
#' 
#' Utility function to make common tests easier.
#'
#' @param ... When used in a validation rule: a bare (unquoted) list of variable names.
#'     When used directly, a comma-separated list of vectors of equal length.
#'
#'
#' @return 
#'   For \code{is_unique} A logical vector that is \code{FALSE} for each record
#'   that has a duplicate.
#'
#' @examples
#' # check that height-weight combinations are unique
#' d <- data.frame(X = c('a','b','c','b'), Y = c('banana','apple','banana','apple'), Z=1:4)
#' v <- validator(is_unique(X, Y))
#' values(confront(d, v))
#' 
#' @export
is_unique <- function(...){
  d <- data.frame(...)
  !duplicated(d) & !duplicated(d, fromLast=TRUE)
}


#' @rdname is_unique
#' @return For \code{all_unique} a single \code{TRUE} or \code{FALSE}.
#' @export
all_unique <- function(...){
  !anyDuplicated(data.frame(...))
}


#### MISSING DATA -------------------------------------------------------------

#' Test for completeness of records
#'
#' Utility function to make common tests easier.
#'
#' @inheritParams is_unique
#' @return 
#'   For \code{is_complete} A logical vector that is \code{FALSE} for each record
#'   that has a duplicate.
#'
#' @examples
#' d <- data.frame(X = c('a','b',NA,'b'), Y = c(NA,'apple','banana','apple'), Z=1:4)
#' v <- validator(is_complete(X, Y))
#' values(confront(d, v))
#'
#' @export
is_complete <- function(...){
  stats::complete.cases(data.frame(...))
}



#' @rdname is_complete
#' @return For \code{all_unique} a single \code{TRUE} or \code{FALSE}.
#' @export 
all_complete <- function(...){
  all(stats::complete.cases(data.frame(...)))
}


