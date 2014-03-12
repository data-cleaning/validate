
# get boolean results from validationResult
boolean <- function(x,...){
  x$  
  
  
}







###############################################################################
# get information from 'validation' objects
#
# status <- function(x,...){
#   UseMethod("status")  
# }
# 
# status.validation <- function(x,...){
#   w <- hasWarning(x)
#   e <- hasError(x)
#   data.frame(
#     restriction = names(x)
#     , exec    = ifelse(e,"error",ifelse(w,"warning","ok"))
#     , type    = types(x)
#     , size    = sizes(x)
#     , pass   = sapply(x, function(x) ifelse(is.null(x[[1]]),0,sum(x[[1]],na.rm=TRUE))  )
#     , fail    = sapply(x, function(x) ifelse(is.null(x[[1]]),0,sum(!x[[1]],na.rm=TRUE)) ) 
#     , nNA     = sapply(x, function(x) ifelse(is.null(x[[1]]),0,sum(is.na(x[[1]]))) )
#     , row.names=NULL
#   )
# }
# 
# results <- function(x,...){
#   UseMethod("results")
# }
# 
# results.validation <- function(x,...){
#   x <- value(x)[!hasError(x)]
#   lengths  <- sapply(x,length)
#   types    <- sapply(x,function(x) class(x)[1])
#   names <- paste0(types,"_x_",lengths)
#   e <- new.env()
#   for ( v in unique(names) ){
#     assign(x=v, value = sapply( x[names==v], Id ), pos=e)
#   }
#   as.list(e)  
# }
# 
# errors <- function(x,...){
#   UseMethod("messages")
# }
# 
# errors.validation <- function(x,...){
#   ie <- sapply(x,inherits,'error')
#   iw <- sapply(x,inherits,'warning')  
#   list(errors=x[ie],warnings=x[iw])
# }
# 
