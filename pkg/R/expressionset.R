#' @include parse.R
NULL


# Superclass for storing verification rules.
setRefClass("expressionset"
  , fields = list(._calls = 'list', ._origin= 'character',._options='function')
  , methods= list(
      show = function() show_expressionset(.self)
    , initialize = function(..., .files=NULL) ini_expressionset(.self,..., .files=.files)
    , calls = function(varlist=NULL,...) get_calls(.self,varlist=varlist,...)
    , expand = function(...) expand_expressionset(.self,...)
    , blocks = function() blocks_expressionset(.self)
    , options = function(...) .self$._options(...)
    , clone_options = function(...) clone_and_merge(.self$._options,...)
  )
)

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


#' @rdname origin
setMethod("origin", signature(x="expressionset"), function(x,...) x$._origin)

#' Convert an expressionset to character
#' @param x an object inheriting from \code{expressionse}, for example \code{\link{validator}} 
#' @param ... Arguments to be passed to or from other methods
#' or \code{\link{indicator}}.
setMethod("as.character","expressionset",  function(x,...) sapply(x$._calls,function(y) paste(deparse(y),collapse=" ")))


#' Extract or set names
#' 
#' @param x An R object
#'
#' @return A \code{character} with names of rules occurring in \code{x}
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
#' @param as how to return variables: 
#' \itemize{
#'   \item{\code{'vector'}} Return the uniqe vector of variables occurring in \code{x}.
#'   \item{\code{'matrix'}} Return a boolean matrix, each row representing a rule, each column representing a variable.
#'   \item{\code{'list'}} Return a named \code{list}, each entry containing a character vector with variable names.
#' }
#' @param dummy Also retrieve transient variables set with the \code{:=} operator.
#'
#' @return By default, a \code{character} vector listing all (non-dummy) variables occuring in \code{x}. 
#'
#' @seealso \code{\link{summary}}
#'
#' @example ../examples/variables.R
setMethod("variables", signature(x="expressionset"), function(x, as=c('vector','matrix','list'), dummy=FALSE, ...){ 
    as <- match.arg(as)
    vars <- lapply(x$calls(expand_assignments=!dummy),var_from_call)
    u <- unique(unlist(vars))

    switch(as
      , 'vector' = u
      , 'list'   = vars
      , 'matrix' = {  
        a <- array(FALSE,dim=c(length(vars),length(u)),dimnames=list(rule=names(vars),variable=u) )
        for (i in seq_along(vars)) a[i,vars[[i]]] <- TRUE
        a
    })
})

#' @section Validator and indicator objects:
#' For these objects, the ruleset is split into subsets (blocks) that are disjunct in the
#' sense that they do not share any variables. For each bloch the number of variables, the number 
#' of rules and the number of rules that are linear are reported.
#' 
#' @return A \code{data.frame} with the information mentioned below information is returned.
#' 
#' @rdname validate-summary
setMethod('summary',signature('expressionset'),function(object,...){
  b <- object$blocks()
  data.frame(
    block = seq_along(b)
    , nvar  = sapply(b,function(i) length(variables(object[i])))
    , rules = sapply(b,sum)
    , linear = sapply(b,function(i) sum(object[i]$is_linear()))
    , row.names=NULL
  )
})


# Internal methods ----

setGeneric("is_vargroup",function(x,...) standardGeneric("is_vargroup"))

setMethod("is_vargroup",signature("expressionset"),function(x,...){
  sapply(x$._calls, vargroup)  
})



# IMPLEMENTATIONS -------------------------------------------------------------

ini_expressionset <- function(.self, ..., .files, .prefix="V",.options=options_manager()){

  L <- as.list(substitute(list(...))[-1])
  .self$._options <- .options
  if ( !is.null(.files) && is.character(.files) ){
    # process include statements.
    filestack <- unlist(lapply(.files,get_filestack)) 
    L <- list()
    ifile <- character(0)
    for ( f in filestack ){ 
      L1 <- read_resfile(f, .self)
      ifile <- c(ifile,rep(f,length(L1)))
      L <- c(L,L1)
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

  
show_expressionset <- function(.self){
  nr <- length(.self$._calls)
  cat(sprintf(
    "Object of class '%s' with %s elements\n",class(.self)[1], nr
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
