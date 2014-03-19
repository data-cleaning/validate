
#  Superclass for storing verification rules.
setRefClass("verifier"
  , fields = list(calls = 'list',origin= 'character')
  , methods= list(
      show = function() .show_verifier(.self)
    , initialize = function(...,files=NULL) .verifier(.self,...,files=files)
  )
)

# retrieve (expanded, vectorized) calls from verifier object
setGeneric('calls',function(x,...) standardGeneric('calls'))

setMethod('calls',signature('verifier'),
  function(x, expand=TRUE, vectorize=TRUE, replace_dollar=TRUE ){
    calls <- x$calls
    if ( expand ) calls <- expand_calls(calls)
    if (vectorize) calls <- lapply(calls, vectorize)
    if (replace_dollar) calls <- lapply(calls, replace_dollar)
    calls
})


# get basic information from verification objects
setGeneric("variables", function(x,...) standardGeneric("variables"))

setGeneric("origin",def=function(x,...) standardGeneric("origin"))

setGeneric("is_vargroup",function(x,...) standardGeneric("is_vargroup"))

setMethod("is_vargroup",signature("verifier"),function(x,...){
  sapply(x$calls, vargroup)  
})

setGeneric("is_linear", def=function(x,...) standardGeneric("is_linear"))

setGeneric("linear_coefficients",def=function(x,...) standardGeneric("linear_coefficients"))

setMethod("origin", signature(x="verifier"), function(x,...) x$origin)

setMethod("as.character","verifier", function(x,...) sapply(x$calls,deparse))

setMethod("names","verifier", function(x) names(x$calls))

setMethod("[",signature("verifier"), function(x,i,...){
  out <- do.call(class(x), x$calls[i])
  out$origin <- x$origin[i]
  out
})

setMethod("variables", signature(x="verifier"), function(x,...){ 
    unique(unlist(lapply(x$calls,var_from_call)))
  }
)



# IMPLEMENTATIONS -------------------------------------------------------------


# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
  generic <- sprintf("%s%04d",prefix,1:length(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given == ""
  given[igen] <- generic[igen]
  given
}


.show_verifier <- function(.self){
  nr <- length(.self$calls)
  cat(sprintf(
    "Reference object of class '%s' with %s elements\n",class(.self)[1], nr
  ))
  if (nr == 0) return(invisible(NULL))
  lab <- names(.self$calls)
  n <- max(nchar(lab))
  lab <- paste0(format(lab,width=n),": ",sapply(.self$calls, call2text))
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







