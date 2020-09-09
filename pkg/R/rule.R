
# RULE OBJECT -----------------------------------------------------------------

#' A rich expression
#' 
#' @section Details:
#' Technically, \code{rule} is a \code{call} object endowed with extra
#' attributes such as a name, a label and a description description, creation
#' time and a reference to its origin. Rule objects are not for direct use by
#' users of the package, but may be of interest for developers of this package,
#' or packages depending on it.
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
   expr          = "language"  # MUST be a 'call'[*]
   , name        = "character"
   , label       = "character" # label description
   , description = "character" # description description
   , origin      = "character" 
   , created     = "POSIXct"
   , meta        = "list"
  )
  , prototype = list(
   expr         = NULL
   , name        = character(0)
   , label       = character(0)
   , description = character(0)
   , origin      = character(0)
   , created     = as.POSIXct(NA)
   , meta        = vector(mode = "list", length = 0)
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

#' Get or set rule metadata
#' 
#' Rule metadata are key-value pairs where the value is a simple (atomic)
#' string or number.
#' 
#' @param x an R object
#' @param ... Arguments to be passed to other methods
#' 
#' @name meta
#' @export
#' @examples 
#' 
#' v <- validator(x > 0, y > 0)
#' 
#' # metadata is recycled over rules
#' meta(v,"foo") <- "bar" 
#' 
#' # assign metadata to a selection of rules
#' meta(v[1],"fu") <- 2
#' 
#' # retrieve metadata as data.frame
#' meta(v)
#' 
#' # retrieve metadata as list
#' meta(v,simplify=TRUE)
#' 
setGeneric("meta",function(x,...) standardGeneric("meta"))





#' Get variable names
#'
#' Generic function that extracts names of variables ocurring
#' in R objects.
#'
#'
#' @param x An R object
#' @param ... Arguments to be passed to other methods.
#' 
#' @family expressionset-methods
#' @name variables
#' @export
setGeneric("variables", function(x,...) standardGeneric("variables"))



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
#' A slot to store where the rule originated, e.g. a filename
#' or \code{"command-line"} for interactively defined rules.
#' 
#'
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' @example ../examples/properties.R
#' @export
setGeneric("origin",def=function(x,...) standardGeneric("origin"))

#' Rule label
#'
#' A short (typically two or three word) description of a rule.
#' 
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
#' @example ../examples/properties.R
#' @export
setGeneric("label", function(x,...) standardGeneric("label"))


#' Rule description
#'
#' A longer (typically one-paragraph) description of a rule.
#'
#' @param x and R object
#' @param ... Arguments to be passed to other methods
#' @return A \code{character} vector.
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
#' @example ../examples/properties.R
#' @export
setGeneric("created", function(x,...) standardGeneric("created"))

#' @rdname meta
#' @param name  \code{[character]} metadata key
#' @param value Value to set
#' @export
setGeneric("meta<-", function(x, name, value) standardGeneric("meta<-"))




#' @rdname origin
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("origin<-",function(x,value) standardGeneric("origin<-"))

#' @rdname label
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("label<-",function(x,value) standardGeneric("label<-"))

#' @rdname description
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("description<-",function(x,value) standardGeneric("description<-"))


#' @rdname created
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setGeneric("created<-",function(x,value) standardGeneric("created<-"))



# S4 METHODS ------------------------------------------------------------------

#' @rdname meta 
setMethod("meta","rule", function(x,...){
  x@meta
})

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
  nm <- nm[nm != "meta"]
  vl <- sapply(nm,function(x) paste0("",format(slot(object,x))))
  fmt <- paste0("\n %-",n,"s: %s")
  cat(sprintf(fmt,nm,vl))
  # meta names and abbreviated type.
  tp <- abbreviate(sapply(meta(object),class),3)
  nm <- names(meta(object))
  meta_str <- paste(sprintf("%s<%s>",nm,tp), collapse=", ")
  cat(sprintf(fmt,"meta",meta_str))
})

setMethod("validating","rule", function(x,...){
  validating_call(x@expr)  
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



#' @rdname meta
#' @export 
setReplaceMethod("meta", c("rule","character"), function(x, name, value){
  x@meta[[name]] <- value
  x
})


#' @rdname names
#' @export 
setReplaceMethod("names",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("name must be 'character' of length 1")
  }
  x@name <- value
  x
})




#' @rdname origin
#' @export 
setReplaceMethod("origin",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("origin must be 'character' of length 1")
  }
  x@origin <- value
  x
})


#' @rdname label
#' @export 
setReplaceMethod("label",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("label must be 'character' of length 1")
  }
  x@label <- value
  x
})

#' @rdname description
#' @export 
setReplaceMethod("description",c("rule","character"),function(x,value){
  if (length(value) > 1){
    stop("description must be 'character' of length 1")
  }
  x@description <- value
  x
})

#' @rdname created
#' @export 
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
    origin = x@origin,
    meta = x@meta
  )
}

