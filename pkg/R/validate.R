
###############################################################################
#' Validate data against a set of restrictions
#'
#' 
#' @param x An object of class \code{\link{restrictionset}}
#' @param dat A data set, e.g. a \code{data.frame}
#' @param ... Optional extra arguments, depending on the method.
#' 
#' @return A List of logical arrays.
#'
#' @export
setGeneric("validate",
  def = function(x, dat, ...) standardGeneric("validate")
)

#' @rdname validate
setMethod("validate",signature(x="restrictionset",dat="data.frame"),
  function(x, dat, ...){
    
    L <- lapply(x$restrictions, factory(eval), dat)
    
    structure(L,class=c("validation","list"))
  }
)

# Martin Morgan's factory; taken from SO.
# Collects results, warnigns and errors.
# http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
factory <- function(fun){
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(res, warn=warn, err=err)
  }
}

# Martin Morgan's helper-outers
.has <- function(x, what) !sapply(lapply(x, "[[", what), is.null)
hasWarning <- function(x) .has(x, "warn")
hasError <- function(x) .has(x, "err")
isClean <- function(x) !(hasError(x) | hasWarning(x))
value <- function(x) sapply(x, "[[", 1)
cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)
# end

types <- function(x) sapply(x,function(x) class(x[[1]]))
sizes <- function(x) sapply(x,function(x) length(x[[1]]))
Id <- function(x) x

###############################################################################
# get information from 'validation' objects
#
status <- function(x,...){
  UseMethod("status")  
}

status.validation <- function(x,...){
  w <- hasWarning(x)
  e <- hasError(x)
  data.frame(
    restriction = names(x)
    , exec    = ifelse(e,"error",ifelse(w,"warning","ok"))
    , type    = types(x)
    , size    = sizes(x)
    , pass   = sapply(x, function(x) ifelse(is.null(x[[1]]),0,sum(x[[1]],na.rm=TRUE))  )
    , fail    = sapply(x, function(x) ifelse(is.null(x[[1]]),0,sum(!x[[1]],na.rm=TRUE)) ) 
    , nNA     = sapply(x, function(x) ifelse(is.null(x[[1]]),0,sum(is.na(x[[1]]))) )
    , row.names=NULL
  )
}

results <- function(x,...){
  UseMethod("results")
}

results.validation <- function(x,...){
  x <- value(x)[!hasError(x)]
  lengths  <- sapply(x,length)
  types    <- sapply(x,function(x) class(x)[1])
  names <- paste0(types,"_x_",lengths)
  e <- new.env()
  for ( v in unique(names) ){
    assign(x=v, value = sapply( x[names==v], Id ), pos=e)
  }
  as.list(e)  
}

errors <- function(x,...){
  UseMethod("messages")
}

errors.validation <- function(x,...){
  ie <- sapply(x,inherits,'error')
  iw <- sapply(x,inherits,'warning')  
  list(errors=x[ie],warnings=x[iw])
}

