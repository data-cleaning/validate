
#  Superclass for storing verification rules.
setRefClass("verifier"
  , fields = list(calls = 'list',origin= 'character')
  , methods= list(
    initialize = function(...,files=NULL) .verifier(.self,...,files=files)
    , show = function() .show_validator(.self) 
    )
)


# get basic information from verification objects
setGeneric("variables", function(x,...) standardGeneric("variables"))

setGeneric("origin",def=function(x,...) standardGeneric("origin"))


# IMPLEMENTATIONS -------------------------------------------------------------

.verifier <- function(.self, ..., files){
  L <- as.list(substitute(list(...))[-1])

  if ( !is.null(file) && is.character(file) ){
    L <- list()
    ifile <- character(0)
    for ( f in file ){ 
      L <- c(L,read_resfile(f))
      ifile <- c(ifile,rep(f,length(L)))
    }
  } else if (length(L)==0){
    return(.self)
  } else {
    names(L) <- extract_names(L)
    ifile <- rep("commandline",length(L))
  }
  names(ifile) <- names(L)
  .self$calls <- L
  .self$origin <- ifile
  .self
}

# get names from a list, replacing empty names values with numbers
extract_names <- function(L){
  generic <- sprintf("V%04d",1:length(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given == ""
  given[igen] <- generic[igen]
  given
}


.show_validator <- function(.self){
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

setMethod("variables", signature(x="verifier"),
  function(x,...){ 
    unique(unlist(lapply(x$calls,var_from_call)))
  }
)

# Extract variable names from a call object
var_from_call <- function( x, vars=character(0) ){
  
  if ( length(x)==1 && is.symbol(x) ) return(deparse(x) )
  
  if (length(x) > 1){
    for ( i in 2:length(x) ) vars <- c(vars,var_from_call(x[[i]]))
  }
  unique(vars)
}


setMethod("origin", signature(x="verifier"), function(x,...) x$origin)

setMethod("as.character","verifier", function(x,...) sapply(x$calls,deparse))

setMethod("names","verifier", function(x) names(x$calls))



