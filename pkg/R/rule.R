
# RULE OBJECT -----------------------------------------------------------------

#' A rich expression
#' 
#' @section Details:
#' Technically, \code{rule} is a \code{call} object endowed with extra attributes such
#' as a name, a label and a description description, creation time and a reference to its origin.
#' Rule objects are not for direct use by users of the package, but may be of interest for
#' developers of this package, or packages depending on it.
#' 
#' @section Exported S4 methods for \code{rule}:
#' \itemize{
#' \item{\code{show}}
#' \item{\code{\link{origin}}}
#' \item{\code{\link{label}}}
#' \item{\code{\link{description}}}
#' \item{\code{\link{created}}}
#' }
#'
#' @section Private S4 methods for \code{rule}:
#' \itemize{
#'  \item{validating}
#'  \item{linear}
#'  \item{expr}
#'  \item{is_tran_assign}
#' }
#' 
#' @section See also:
#' \itemize{
#'  \item{\code{\link{expressionset-class}}}
#' }
#'
#' @keywords internal
rule <- setClass("rule",
  slots = c(
   expr         = "language"  # MUST be a 'call'[*]
   , name       = "character"
   , label      = "character" # label description
   , description       = "character" # description description
   , origin     = "character" 
   , created    = "POSIXct"
  )
  , prototype = list(
   expr         = NULL
   , name       = character(0)
   , label      = character(0)
   , description       = character(0)
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
#' @seealso 
#' \itemize{
#' \item{\code{\link{names,expressionset-method}}, \code{\link{length,expressionset-method}}}
#' \item{\code{\link{description}}, \code{\link{label}}, \code{\link{created}}, \code{\link{origin}}}
#' }
#' @name variables
#' @export
setGeneric("variables", function(x,...) standardGeneric("variables"))


#' Set origin
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("origin<-",function(x,value) standardGeneric("origin<-"))

#' Set label
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("label<-",function(x,value) standardGeneric("label<-"))

#' Set description
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("description<-",function(x,value) standardGeneric("description<-"))


#' Set creation timestamp
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("created<-",function(x,value) standardGeneric("created<-"))


setGeneric("validating",function(x,...) standardGeneric('validating'))

setGeneric("linear",function(x,...) standardGeneric("linear"))

#' Get expressions
#'
#' @param x Object
#' @param ... options to be passed to other functions
#' @keywords internal
#' @export
#' @rdname expr
setGeneric("expr",def=function(x,...) standardGeneric("expr"))

# check for transient assignments (:=)
setGeneric("is_tran_assign", function(x,...) standardGeneric("is_tran_assign"))



#' Origin of rules
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#'
#' @seealso 
#' \itemize{
#' \item{\code{\link{names,expressionset-method}}, \code{\link{length,expressionset-method}}}
#' \item{\code{\link{description}}, \code{\link{label}}, \code{\link{created}}, \code{\link{variables}}}
#' }
#'  
#' @example ../examples/properties.R
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))

#' label description of rules
#' 
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' 
#' @seealso 
#' \itemize{
#' \item{\code{\link{names,expressionset-method}}, \code{\link{length,expressionset-method}}}
#' \item{\code{\link{description}}, \code{\link{created}}, \code{\link{origin}}, \code{\link{variables}}}
#' }
#' @example ../examples/properties.R
#' @export
setGeneric("label", function(x,...) standardGeneric("label"))


#' description description
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' 
#' @seealso 
#' \itemize{
#' \item{\code{\link{names,expressionset-method}}, \code{\link{length,expressionset-method}}}
#' \item{\code{\link{label}}, \code{\link{created}}, \code{\link{origin}}, \code{\link{variables}}}
#' }
#' @example ../examples/properties.R
#' @export
setGeneric("description", function(x,...) standardGeneric("description"))


#' Creation timestamp
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{POSIXct} vector.
#' 
#' @seealso 
#' \itemize{
#' \item{\code{\link{names,expressionset-method}}, \code{\link{length,expressionset-method}}}
#' \item{\code{\link{description}}, \code{\link{label}}, \code{\link{origin}} \code{\link{variables}}}
#' }
#' @example ../examples/properties.R
#' @export
setGeneric("created", function(x,...) standardGeneric("created"))



# S4 METHODS ------------------------------------------------------------------
#' @rdname expr
setMethod("expr","rule",function(x,...) x@expr)

#' @describeIn variables Retrieve unique variable names
setMethod("variables","rule", function(x,...){
  var_from_call(x@expr)
})

#' @describeIn variables Alias to \code{names.list}
setMethod('variables',signature('list'), function(x,...) names(x))

#' @describeIn variables Alias to \code{names.data.frame}
setMethod('variables',signature('data.frame'), function(x,...) names(x))

#' @describeIn variables Alias to \code{ls}
setMethod('variables',signature('environment'), function(x,...) ls(x))



# full print method for rules
setMethod("show", "rule", function(object){
  cat(sprintf("\nObject of class %s.",class(object)))
  nm <- slotNames(object)
  n <- max(nchar(nm))
  vl <- sapply(nm,function(x) paste0("",format(slot(object,x))))
  fmt <- paste0("\n %-",n,"s: %s")
  cat(sprintf(fmt,nm,vl))
})

setMethod("validating","rule", function(x,...){
 !is.null(variables(x)) && validating_call(x@expr)  
})


setMethod("linear","rule",function(x,...){
  linear_call(x@expr)
})

#' @rdname origin
setMethod("origin","rule",function(x,...){
  setNames(x@origin, x@name)
})

#' @rdname label
setMethod("label","rule",function(x,...) setNames(paste0("",x@label),x@name) )

#' @rdname description
setMethod("description", "rule", function(x,...) setNames( paste0("",x@description), x@name) )

#' @rdname created
setMethod("created", "rule", function(x,...) setNames( x@created,x@name) )

setMethod("is_tran_assign","rule", function(x){
  x@expr[[1]] == ":="
})


#' Set names
#'
#' @param x Object
#' @param value Value to set
#' @export 
#' @keywords internal
setReplaceMethod("names",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("name must be 'character' of length 1")
  }
  x@name <- value
  x
})

#' Set origin
#'
#' @param x Object
#' @param value Value to set
#' @export 
#' @keywords internal
setReplaceMethod("origin",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("origin must be 'character' of length 1")
  }
  x@origin <- value
  x
})

#' Set label
#'
#' @param x Object
#' @param value Value to set
#' @export 
#' @keywords internal
setReplaceMethod("label",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("label must be 'character' of length 1")
  }
  x@label <- value
  x
})

#' Set description
#'
#' @param x Object
#' @param value Value to set
#' @export 
#' @keywords internal
setReplaceMethod("description",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("description must be 'character' of length 1")
  }
  x@description <- value
  x
})

#' Set creation timestamp
#'
#' @param x Object
#' @param value Value to set
#' @export 
#' @keywords internal
setReplaceMethod("created",c("rule","POSIXct"),function(x,value){
  if (length(value) > 1){
    stop("timestamp must be 'POSIXct' of length 1")
  }
  x@created <- value
  x
})




# handy for rule to yaml/json
as.list.rule <- function(x, expr_as_text = TRUE, ...){
  expr <- x@expr
  if (expr_as_text){
    expr <- deparse(expr, width.cutoff = 500L)
  }
  list(
    expr=expr,
    name = x@name,
    label = x@label,
    description = x@description, 
    created = x@created,
    origin = x@origin
  )
}

