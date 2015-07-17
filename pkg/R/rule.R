
# RULE OBJECT -----------------------------------------------------------------
# A rule is a call object endowed with extra attributes
rule <- setClass("rule",
  slots = c(
   expr         = "language"  # MUST be a 'call'[*]
   , name       = "character"
   , short      = "character" # short description
   , long       = "character" # long description
   , origin     = "character" 
   , created    = "POSIXct"
  )
  , prototype = list(
   expr         = NULL
   , name       = character(0)
   , short      = character(0)
   , long       = character(0)
   , origin     = character(0)
   , created    = as.POSIXct(NA)
  )
)


#[*] Peeling off an expression always yields an object of 'mode' call, but not
#    of 'type' call. For example:
#
# p <-  parse(text="x + y")[[1]]
# class(p)
# [1] "call"
#
# p <- parse(text="if (A) B))[[1]]
# parse(p) 
# [1] "if"
# is.call(p)
# [1] TRUE
#
#


# S4 GENERICS -----------------------------------------------------------------

#' Extract variable names
#'
#' @param x An R object
#' @param ... Arguments to be passed to other methods.
#' 
#' @export
setGeneric("variables", function(x,...) standardGeneric("variables"))


# x: object holding a call
# y: object holding validating symbols
setGeneric("validating",function(x,...) standardGeneric('validating'))

setGeneric("linear",function(x,...) standardGeneric("linear"))

# retrieve expression
setGeneric("expr",def=function(x,...) standardGeneric("expr"))

#' Origin of rules
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#'
#' @seealso \code{\link{short}}, \code{\link{long}}, \code{\link{created}}
#'   
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))

#' Short description of rules
#' 
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' @seealso \code{\link{origin}}, \code{\link{long}}, \code{\link{created}}
#' @export
setGeneric("short", function(x,...) standardGeneric("short"))


#' Long description of rules
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' @seealso \code{\link{origin}}, \code{\link{short}}, \code{\link{created}}
#' @export
setGeneric("long", function(x,...) standardGeneric("long"))


#' Creation time of rules
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{POSIXct} vector.
#' @seealso \code{\link{origin}}, \code{\link{short}}, \code{\link{created}}
#' @export
#' @rdname created
setGeneric("created", function(x,...) standardGeneric("created"))


# S4 METHODS ------------------------------------------------------------------


setMethod("expr","rule",function(x,...) x@expr)

#' @rdname variables
setMethod("variables","rule", function(x,...){
  var_from_call(x@expr)
})

# full print method for rules
setMethod("show", "rule", function(object){
  cat(sprintf("\nObject of class %s.",class(object)))
  nm <- slotNames(object)
  n <- max(nchar(nm))
  vl <- sapply(nm,function(x) format(slot(object,x)) )
  fmt <- paste0("\n %-",n,"s: %s")
  cat(sprintf(fmt,nm,vl))
})

setMethod("validating","rule", function(x,y,...){
  validating_call(x@expr,y)  
})

setMethod("linear","rule",function(x,...){
  linear_call(x@expr)
})

#' @rdname origin
setMethod("origin","rule",function(x,...){
  x@origin
})

#' @rdname short
setMethod("short","rule",function(x,...) x@short)

#' @rdname long
setMethod("long", "rule", function(x,...) x@long)

#' @rdname created
setMethod("created", "rule", function(x,...) x@created)




