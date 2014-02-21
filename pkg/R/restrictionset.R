#' Create or manipulate a set of restrictions
#' 
#' @usage restrictionset(...,files=NULL)
#' @param ... A (named) comma-separated list of expressions.
#' @param files A Character vector
#'
#' @return For \code{restrictionset}, a reference object of class 'restrictionset'.
#'
#'
#' @seealso \code{\link{vars}}, \code{\link{validate}}
#' 
#'
#' @exportClass restrictionset
restrictionset <- setRefClass("restrictionset"
  , contains = 'validator'
  , methods = list(
    initialize = function(...,files=NULL)  .restrictionset(.self,...,files=files)    
    )
)


parse_restrictions <- function(x){
  x <- lapply(x,extract_datamodel)
  lapply(x,vectorize)
}

  
## ----------------------------------------------------------------------------
## internal methods: constructor and show
.restrictionset <- function(.self, ..., files){
  .validator(.self,...,files=files)
  
  i <- sapply(.self$calls, is.validating)
  if ( !all(i) ){
    warning(paste(
      "The following rules are not validation rules and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  }
  .self$calls <- parse_restrictions(.self$calls[i])
  .self$origin <- .self$origin[i]
  .self
}


## ----------------------------------------------------------------------------
## overloaded methods for 'restrictionset'

#' @rdname restrictionset
#' @param x Object of class 'restrictionset'
setMethod("names","restrictionset", function(x) names(x$restrictions))

#' @rdname restrictionset
#' @param i An index, or index vector of class \code{logical}, \code{character} or \code{numeric}
#' @param j ignored
#' @param drop ignored
setMethod("[[","restrictionset", function(x, i, j, ...) x$restrictions[[i]])

#' @rdname restrictionset
setMethod("[","restrictionset", function(x,i,j,...){ 
  r <- restrictionset()
  r$restrictions <- x$restrictions[i]
  r$origin <- x$origin[i]
  r
})

#' @rdname restrictionset
setMethod("as.character","restrictionset", 
  function(x,...) sapply(x$restrictions,deparse)
)

#' @rdname restrictionset
setMethod("as.data.frame","restrictionset",
  function(x, row.names=NULL, optional=FALSE, ...){
    data.frame(
      restriction = sapply(x$restrictions,deparse)
      , name = names(x)
      , origin = x$origin
      , row.names=row.names
      , stringsAsFactors=FALSE
    )
  }
)


## ----------------------------------------------------------------------------
## get basic info from 'restrictionset'

#' Get the origin of restrictions
#' 
#' @param x An R object of class \code{\link{restrictionset}}
#' @param ... Extra arguments (currently unimplemented)
#' @export 
setGeneric("origin",def=function(x,...) standardGeneric("origin"))

#' @rdname origin
setMethod("origin", signature(x="restrictionset"), function(x,...) x$origin)

#' Retrieve variable constrained by a set of restrictions
#'
#' @param x An object of class \code{\link{restrictionset}}
#' @param ... Optional extra arguments that momentarily have no meaning
#'
#' @return A \code{character} vector
#' @export
setGeneric("vars", function(x,...) standardGeneric("vars"))

#' @rdname vars
setMethod("vars", signature(x="restrictionset"),
  function(x,...){ 
    unique(unlist(lapply(x$restrictions,var_from_call)))
  }
)

# Extract variable names from a call object
var_from_call <- function(x,vars=character(0)){
  
  if ( length(x)==1 && is.symbol(x) ) return(deparse(x) )
  
  if (length(x) > 1){
    for ( i in 2:length(x) ) vars <- c(vars,var_from_call(x[[i]]))
  }
  return(unique(vars))  
}
