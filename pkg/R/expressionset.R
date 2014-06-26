#' @import methods
NULL

# Superclass for storing verification rules.
setRefClass("expressionset"
  , fields = list(._calls = 'list',origin= 'character')
  , methods= list(
      show = function() show_expressionset(.self)
    , initialize = function(...,files=NULL) ini_expressionset(.self,...,files=files)
    , calls = function(...) get_calls(.self,...)
  )
)

# Retrieve calls from object
#  
# This function is exported mostly as a utility for other packages depending on \code{validate}. 
# 
# @param x An R object
# @param ... arguments to be passed to other methods
# @return A \code{list} of calls
# @export 
#setGeneric('calls',function(x,...) standardGeneric('calls'))

# @param expand_assignments Substitute assignments?
# @param expand_groups Expand groups?
# @param vectorize Vectorize if-statements?
# @param replace_dollar Replace dollar with bracket index?
# @rdname calls
#setMethod('calls',signature('expressionset'),
#  function(x, ..., expand_assignments=FALSE, expand_groups=TRUE, vectorize=TRUE, replace_dollar=TRUE ){
#    calls <- x$calls
#    if ( expand_assignments )  calls <- expand_assignments(calls)
#    if ( expand_groups ) calls <- expand_groups(calls)
#    if ( vectorize ) calls <- lapply(calls, vectorize)
#    if ( replace_dollar ) calls <- lapply(calls, replace_dollar)
#    calls
#})

# @param expand_assignments Substitute assignments?
# @param expand_groups Expand groups?
# @param vectorize Vectorize if-statements?
# @param replace_dollar Replace dollar with bracket index?
# @rdname calls
#
get_calls <- function(x, ..., expand_assignments=FALSE
    , expand_groups=TRUE, vectorize=TRUE, replace_dollar=TRUE ){
  calls <- x$._calls
  if ( expand_assignments )  calls <- expand_assignments(calls)
  if ( expand_groups ) calls <- expand_groups(calls)
  if ( vectorize ) calls <- lapply(calls, vectorize)
  if ( replace_dollar ) calls <- lapply(calls, replace_dollar)
  calls
}


# get basic information from verification objects

#' Extract variable names
#'
#' @param x An R object
#' @param ... Arguments to be passed to other methods.
#' 
#' @export
setGeneric("variables", function(x,...) standardGeneric("variables"))

#' Find out where expressions were defined
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))


#' Check for linear expressions
#' @param x An R object 
#' @param ... Arguments to be passed to other methods.
#' @return A \code{logical} vector
#' @export
setGeneric("is_linear", def=function(x,...) standardGeneric("is_linear"))


#' Extract linear coeffiecients from linear expressions
#'
#' @section Details: Linear expressions are expressions of the form \eqn{\boldsymbol{Ay}} or
#' \eqn{\boldsymbol{Ay}\odot\boldsymbol{b}}, where \eqn{\odot\in\{<,\leq,=,\geq,>\}}.
#' This function uses \code{\link{is_linear}} to find linear expressions in \code{x} and returns
#' the corresponding coefficients and possibly the operators. 
#'
#' @param x An R object
#' @param ... Arguments to be passed to other methods
#'
#' @return A list, containing matrix \eqn{\boldsymbol{A}}, and where possible matrix \eqn{\boldsymbol{b}} 
#'  and a vector with comparison operators.
#'
#' @export 
setGeneric("linear_coefficients",def=function(x,...) standardGeneric("linear_coefficients"))

#' @rdname origin
setMethod("origin", signature(x="expressionset"), function(x,...) x$origin)

#' @rdname variables
setMethod("as.character","expressionset", function(x,...) sapply(x$._calls,deparse))


#' Extract names
#' 
#' @param x An R object
#'
#' @return A \code{character} with names of variables occurring in \code{x}
#' @export
setMethod("names","expressionset", function(x) names(x$._calls))

#' Select
#' 
#' @param x An R object
#' @param i an index (numeric, boolean, character)
#' @param j not implemented
#' @param drop not implemented
#' @param ... Arguments to be passed to other methods
#' 
#' @return An new object, of the same class as \code{x} subsetted according to \code{i}.
#' 
#' @export
#' @rdname select
setMethod("[",signature("expressionset"), function(x,i,j,...,drop=TRUE){
  out <- do.call(class(x), x$._calls[i])
  out$origin <- x$origin[i]
  out
})

#' @rdname variables
#' @param matrix Return boolean matrix, one row for each rule, one column for each variable.
#' @param dummy Also retrieve transient variables set with the \code{:=} operator.
#'
#' @return By default, a \code{character} vector listing all (non-dummy) variables occuring in \code{x}. 
#'
#' @example ../examples/variables.R
setMethod("variables", signature(x="expressionset"), function(x, matrix=FALSE, dummy=FALSE, ...){ 
    vars <- lapply(x$calls(expand_assignments=!dummy),var_from_call)
    u <- unique(unlist(vars))
    if ( !matrix )
      u
    else {
      a <- array(FALSE,dim=c(length(vars),length(u)),dimnames=list(rule=names(vars),variable=u) )
      for (i in seq_along(vars)) a[i,vars[[i]]] <- TRUE
      a
    }
  }
)

# Internal methods ----

setGeneric("is_vargroup",function(x,...) standardGeneric("is_vargroup"))

setMethod("is_vargroup",signature("expressionset"),function(x,...){
  sapply(x$._calls, vargroup)  
})



# IMPLEMENTATIONS -------------------------------------------------------------

ini_expressionset <- function(.self, ..., files,prefix="V"){
  L <- as.list(substitute(list(...))[-1])
  
  if ( !is.null(files) && is.character(files) ){
    L <- list()
    ifile <- character(0)
    for ( f in files ){ 
      L <- c(L,read_resfile(f))
      ifile <- c(ifile,rep(f,length(L)))
    }
  } else if (length(L)==0){
    return(.self)
  } else {
    names(L) <- extract_names(L,prefix=prefix)
    ifile <- rep("command-line",length(L))
  }
  names(ifile) <- names(L)
  .self$._calls <- L
  .self$origin <- ifile
  .self
}


# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
  generic <- sprintf("%s%04d",prefix,1:length(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given == ""
  given[igen] <- generic[igen]
  given
}

  
show_expressionset <- function(.self){
  nr <- length(.self$._calls)
  cat(sprintf(
    "Reference object of class '%s' with %s elements\n",class(.self)[1], nr
  ))
  if (nr == 0) return(invisible(NULL))
  lab <- names(.self$._calls)
  n <- max(nchar(lab))
  lab <- paste0(format(lab,width=n),": ",sapply(.self$._calls, call2text))
  cat(noquote(paste(lab,collapse="\n")))
  cat("\n")
}

# from call to oneliner text
call2text <- function(x){
  gsub("[[:blank:]]+"," ",paste(deparse(x),collapse=" "))
}

# Extract variable names from a call object
var_from_call <- function( x, vars=character(0) ){
  
  if ( length(x)==1 && is.symbol(x) ) return(deparse(x) )
  
  if (length(x) > 1){
    for ( i in 2:length(x) ) vars <- c(vars,var_from_call(x[[i]]))
  }
  unique(vars)
}
