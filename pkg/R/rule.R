
# RULE OBJECT -----------------------------------------------------------------
# A rule is a call object endowed with extra attributes
rule <- setClass("rule",
  slots = c(
   call         = "language"  # MUST be a 'call'[*]
   , name       = "character"
   , short      = "character" # short description
   , long       = "character" # long description
   , origin     = "character" 
   , created    = "POSIXct"
  )
  , prototype = list(
   call         = NULL
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




# S4 METHODS ------------------------------------------------------------------

#' @rdname variables
setMethod("variables","rule", function(x,...){
  var_from_call(x@call)
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

setMethod("validating",c("rule"), function(x,y,...){
  validating_call(x@call,y)  
})

setMethod("linear","rule",function(x,...){
  linear_call(x@call)
})









