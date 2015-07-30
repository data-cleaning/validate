
# RULE OBJECT -----------------------------------------------------------------

#' A rich expression
#' 
#' @section Details:
#' Technically, \code{rule} is a \code{call} object endowed with extra attributes such
#' as a name, a title and a description description, creation time and a reference to its origin.
#' Rule objects are not for direct use by users of the package, but may be of interest for
#' developers of this package, or packages depending on it.
#' 
#' @section Exported S4 methods for \code{rule}:
#' \itemize{
#' \item{\code{show}}
#' \item{\code{\link{origin}}}
#' \item{\code{\link{title}}}
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
   , title      = "character" # title description
   , description       = "character" # description description
   , origin     = "character" 
   , created    = "POSIXct"
  )
  , prototype = list(
   expr         = NULL
   , name       = character(0)
   , title      = character(0)
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
#' \item{\code{\link{description}}, \code{\link{title}}, \code{\link{created}}, \code{\link{origin}}}
#' }
#' @name variables
#' @export
setGeneric("variables", function(x,...) standardGeneric("variables"))


setGeneric("validating",function(x,...) standardGeneric('validating'))

setGeneric("linear",function(x,...) standardGeneric("linear"))

# retrieve expression
setGeneric("expr",def=function(x,...) standardGeneric("expr"))

# check for transient assignments (:=)
setGeneric("is_tran_assign", function(x,...) standardGeneric("is_tran_assign"))

setGeneric("group_definition", function(x,...) standardGeneric('group_definition'))


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
#' \item{\code{\link{description}}, \code{\link{title}}, \code{\link{created}}, \code{\link{variables}}}
#' }
#'  
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))

#' title description of rules
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
#' @export
setGeneric("title", function(x,...) standardGeneric("title"))


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
#' \item{\code{\link{title}}, \code{\link{created}}, \code{\link{origin}}, \code{\link{variables}}}
#' }
#' @export
setGeneric("description", function(x,...) standardGeneric("description"))


#' Creation time
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{POSIXct} vector.
#' 
#' @seealso 
#' \itemize{
#' \item{\code{\link{names,expressionset-method}}, \code{\link{length,expressionset-method}}}
#' \item{\code{\link{description}}, \code{\link{title}}, \code{\link{origin}} \code{\link{variables}}}
#' }
#' @export
setGeneric("created", function(x,...) standardGeneric("created"))


# S4 METHODS ------------------------------------------------------------------

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
  vl <- sapply(nm,function(x) format(slot(object,x)) )
  fmt <- paste0("\n %-",n,"s: %s")
  cat(sprintf(fmt,nm,vl))
})

setMethod("validating","rule", function(x,y,...){
  validating_call(x@expr,y)  
})

setMethod("group_definition","rule",function(x,...){
  defines_group(x@expr)
})

setMethod("linear","rule",function(x,...){
  linear_call(x@expr)
})

#' @rdname origin
setMethod("origin","rule",function(x,...){
  x@origin
})

#' @rdname title
setMethod("title","rule",function(x,...) x@title)

#' @rdname description
setMethod("description", "rule", function(x,...) x@description)

#' @rdname created
setMethod("created", "rule", function(x,...) x@created)

setMethod("is_tran_assign","rule", function(x){
  x@expr[[1]] == ":="
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
    title = x@title,
    description = x@description, 
    created = x@created,
    origin = x@origin
  )
}

