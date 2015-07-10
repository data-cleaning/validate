#' @include parse.R
#' @include sugar.R
#' @include rule.R
NULL

#  library(settings)
#  source('pkg/R/sugar.R')
#  source('pkg/R/parse.R')
#  source('pkg/R/rule.R')

#### EXPRESSIONSET OBJECT -----------------------------------------------------
# superclass storing a set of rich expressions
expressionset <- setRefClass("expressionset"
  , fields = list(
        rules = "list"
      , ._options = "function"
  )
  , methods= list(
        show = function() show_expressionset(.self)
      , calls      = function(varlist=NULL,...) get_calls(.self, varlist=varlist,...)
      , blocks     = function() blocks_expressionset(.self)
      , options = function(...) .self$._options(...)
      , clone_options = function(...) clone_and_merge(.self$._options,...)
  )
)


## Service for filling an expressionset from commandline
ini_expressionset_cli <- function(obj, ..., .prefix="R"){  

  L <- as.list(substitute(list(...))[-1])
  nm <- extract_names(L, prefix = .prefix)
  cr <- Sys.time()
  R <- vector(length(L), mode='list')
  for ( i in seq_along(L) ){
    R[[i]] <- rule(
        call = L[[i]]
      , name = nm[i]
      , origin="command-line"
      , created = cr
      )
  }
  obj$rules <- R
}

## TODO: Service for creating child objects of expressionset from file
ini_expressionset_yml <- function(obj,file,.prefix="R"){
  obj
}

# Get sequence of files to be processed from include statements.
get_filestack <- function(file){
  # Detecting include statements.
  tag <- "#[[:blank:]]+@validate[[:blank:]]+include[[:blank:]]+"
  
  f <- function(fl, det=character(0)){
    det <- c(fl,det)
    if ( fl %in% det[-1])
      stop(sprintf("Cyclic dependency detected in %s\n%s\n",fl,paste(rev(det),collapse=" -> ")))
    r <- readLines(fl)
    L <- grep(tag,r,value=TRUE)
    if ( length(L) > 0)
      L <- gsub(paste0(tag,"|[[:blank:]]*$"),"",L) # also remove trailing blanks.
    for ( x in L )
      f(x,det)
    filestack <<- c(filestack,fl)
  }
  filestack <- character(0)
  f(file)
  filestack
}







show_expressionset <- function(obj){
  nr <- length(obj$rules)
  cat(sprintf(
    "Object of class '%s' with %s elements:\n",class(obj)[1], nr
  ))
  if (nr == 0) return(invisible(NULL))
  lab <- names(obj)
  n <- max(nchar(lab))
  lab <- paste0(" ",format(lab,width=n),": ",sapply(obj$calls(), call2text))
  cat(noquote(paste(lab,collapse="\n")))
  cat("\n")
}




# from call to oneliner text
call2text <- function(x){
  gsub("[[:blank:]]+"," ",paste(deparse(x),collapse=" "))
}

# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
  npos <- max(1,ceiling(log10(length(L)+1)))
  fmt <- paste0("%s%0",npos,"d")
  generic <- sprintf(fmt,prefix,seq_along(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given %in% c("", NA)
  given[igen] <- generic[igen]
  make.names(given, unique=T)
}


# @param expand_assignments Substitute assignments?
# @param expand_groups Expand groups?
# @param varlist: a character vector of variables to search through 
#    when groups are defined with regexps.
# @param vectorize Vectorize if-statements?
# @param replace_dollar Replace dollar with bracket index?
# 
#
get_calls <- function(x, ..., expand_assignments=FALSE
    , expand_groups=TRUE, vectorize=TRUE, replace_dollar=TRUE, varlist=NULL ){
  calls <- lapply(x$rules, function(y) y@call )
  if ( expand_assignments )  calls <- expand_assignments(calls)
  if ( expand_groups ) calls <- expand_groups(calls, varlist=varlist)
  if ( vectorize ) calls <- lapply(calls, vectorize)
  if ( replace_dollar ) calls <- lapply(calls, replace_dollar)
  calls
}

blocks_expressionset <- function(x){
  varlist <- variables(x)
  
  varblock <- function(v,vlist){
    sapply(vlist, function(x) any(v %in% x))
  }
  
  # compute variable blocks
  blocks <- new.env()
  b <- 0
  V <- varlist
  while( length(V) >= 1 ){
    b <- b+1
    i <- varblock(V[[1]],V)
    blocks[[paste0('block',b)]] <- unique(unlist(V[i]))
    V <- V[!i]
  }
  blocks <- as.list(blocks)
  
  # logical, indicating rule blocks.
  lapply(blocks,function(b) sapply(varlist,function(v) any(v %in% b) ))
}



# S4 GENERICS -----------------------------------------------------------------




#' Create a summary
#' @rdname validate-summary
setGeneric('summary')


#' Find out where expressions were defined
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' 
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))



# S4 IMPLEMENTATIONS ----------------------------------------------------------


setMethod("variables", "expressionset", function(x,...){
  lapply(x$rules, variables)
})

#' @rdname validate_options
setMethod('validate_options','expressionset',function(x=NULL,...){
  if (settings::is_setting(...)){
    x$._options <- clone_and_merge(x$._options,...)
  } else {
    x$._options(...)
  }
})

#' @rdname validate_options
setMethod('validate_reset','expressionset',function(x=NULL){
  settings::reset(x$._options)
})

#' @rdname origin
setMethod("origin", signature(x="expressionset"), function(x,...) x$._origin)

#' Convert an expressionset to character
#' @param x an object inheriting from \code{expressionse}, for example \code{\link{validator}} 
#' @param ... Arguments to be passed to or from other methods
#' or \code{\link{indicator}}.
setMethod("as.character","expressionset",  function(x,...) sapply(x$calls(),function(y) paste(deparse(y),collapse=" ")))


#' Extract or set names
#' 
#' @param x An R object
#'
#' @return A \code{character} with names of rules occurring in \code{x}
#' @export
setMethod("names","expressionset",function(x){
  sapply(x$rules, function(rule) rule@name)
})

setMethod("validating", "expressionset", function(x,...){
  allowed_symbols <- x$options("validator_symbols")
  sapply(x$rules,validating,allowed_symbols)
})

setMethod("linear","expressionset", function(x,...){
  sapply(x$rules, linear)
})


#' Select a subset
#' 
#' @section Details:
#' The \code{options} attribute will be cloned
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
  if (is.character(i)){
    i <- i %in% names(x)
  }
  out <- new(class(x))
  out$rules <- x$rules[i]
  out$._options = clone_and_merge(x$._options)
  out
})

#' @param exact Not implemented
#' @rdname select
setMethod("[[",signature("expressionset"), function(x,i,j,...,exact=TRUE){
  x$rules[[i]]
})




# demonstruction
# L <- list(
#   rule(call = expression(x + y == z)[[1]],  name="aap")
#  , rule(call = expression(p + q == z)[[1]], name="noot")
#  , rule(call = expression(a*b == c)[[1]],   name="mies")
# )
# # 
# r <- expressionset(rules=L,._options=options_manager())



