#' @import methods
NULL

# Superclass for storing verification rules.
setRefClass("expressionset"
  , fields = list(._calls = 'list', ._origin= 'character')
  , methods= list(
      show = function() show_expressionset(.self)
    , initialize = function(..., .files=NULL) ini_expressionset(.self,..., .files=.files)
    , calls = function(varlist=NULL,...) get_calls(.self,varlist=varlist,...)
    , expand = function(...) expand_expressionset(.self,...)
    , blocks = function() blocks_expressionset(.self)
  )
)

expand_expressionset <- function(x,...){
  x$._calls <- x$calls(expand_assignments=TRUE,...)
  x$._origin <- rep("expanded",length(x$._calls))
  invisible(NULL)
}

blocks_expressionset <- function(x){
  varlist <- lapply(x$._calls,var_from_call)

  varblock <- function(v,vlist){
    sapply(vlist, function(x) any(v %in% x))
  }
  # compute variable blocks
  blocks <- new.env()
  b <- 0
  while(length(varlist)>=1){
      b <- b+1
      i <- varblock(varlist[[1]],varlist)
      blocks[[paste0('block',b)]] <- unique(unlist(varlist[i]))
      varlist <- varlist[!i]
  }
  blocks <- as.list(blocks)
  # logical, indicating rule blocks.
  lapply(blocks,function(b) sapply(b,function(v) any(v %in% b) ))
}



# @param expand_assignments Substitute assignments?
# @param expand_groups Expand groups?
# @param varlist: a character vector of variables to search through 
#    when groups are defined with regexps.
# @param vectorize Vectorize if-statements?
# @param replace_dollar Replace dollar with bracket index?
# @rdname calls
#
get_calls <- function(x, ..., expand_assignments=FALSE
    , expand_groups=TRUE, vectorize=TRUE, replace_dollar=TRUE, varlist=NULL ){
  calls <- x$._calls
  if ( expand_assignments )  calls <- expand_assignments(calls)
  if ( expand_groups ) calls <- expand_groups(calls, varlist=varlist)
  if ( vectorize ) calls <- lapply(calls, vectorize)
  if ( replace_dollar ) calls <- lapply(calls, replace_dollar)
  calls
}

# get basic information from expressionset objects

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
#' 
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))



#' @rdname origin
setMethod("origin", signature(x="expressionset"), function(x,...) x$._origin)

#' Convert an expressionset to character
#' @param x an object inheriting from \code{expressionse}, for example \code{\link{validator}} or \code{\link{indicator}}.
setMethod("as.character","expressionset", function(x,...) sapply(x$._calls,deparse))


#' Extract or set names
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
  out$._origin <- x$._origin[i]
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

ini_expressionset <- function(.self, ..., .files, .prefix="V"){
  L <- as.list(substitute(list(...))[-1])
  
  if ( !is.null(.files) && is.character(.files) ){
    L <- list()
    ifile <- character(0)
    for ( f in .files ){ 
      L <- c(L,read_resfile(f))
      ifile <- c(ifile,rep(f,length(L)))
    }
  } else if (length(L)==0){
    return(.self)
  } else {
    names(L) <- extract_names(L,prefix=.prefix)
    ifile <- rep("command-line",length(L))
  }
  names(ifile) <- names(L)
  .self$._calls <- L
  .self$._origin <- ifile
  .self
}


# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
  npos <- max(1,ceiling(log10(length(L)+1)))
  fmt <- paste0("%s%0",npos,"d")
  generic <- sprintf(fmt,prefix,1:length(L))
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
