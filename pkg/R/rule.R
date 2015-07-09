library(settings)
library(R6)


# A rule is a call object endowed with extra attributes
rule <- setClass("rule",
  slots = c(
    call         = "call"
    , name       = "character"
    , short_note = "character"
    , long_note  = "character"
    , origin     = "character"
    , created    = "POSIXct"
    )
  , prototype = list(
    call         = NULL
    , name       = ""
    , short_note = ""
    , long_not   = ""
    , origin     = ""
    , created    = as.POSIXct(NA)
    )
)

setMethod("show", "rule", function(object){
  cat(sprintf("%-5s: %s (%s)"
        , object@name 
        , deparse(object@call)
        , object@short_note)
      )
})

ruleset <- R6Class("ruleset", public = list(
  rules = list()
  , initialize = function(..., .file) ini_ruleset(self, ..., .file = .file)
  , blocks = function() blocks_ruleset(self)
))

# to allow for S4 overloading
setOldClass(c("ruleset","R6"))

ini_ruleset <- function(obj, ..., .file){  
  prefix = "V"
  
  if ( missing(.file) ){
    L <- as.list(substitute(list(...))[-1])
    nm <- extract_names(L, prefix=prefix)
    cr <- Sys.time()
    R <- vector(length(L), mode='list')
    for ( i in seq_along(L) ){
      R[[i]] <- rule(
          call = L[[i]]
        , name = nm[i]
        , origin="command-line"
        , created = cr
        )
    }
  } # else: parse from (yaml) file
  obj$rules <- R
}


#setGeneric("variables",function(object) standardGeneric("variables"))

setMethod("variables","rule", function(x,...){
  var_from_call(x@call)
})

setMethod("variables", "ruleset", function(x,...){
  lapply(x$rules, variables)
})



blocks_ruleset <- function(x){
  varlist <- variables(x)
  
  varblock <- function(v,vlist){
    sapply(vlist, function(x) any(v %in% x))
  }
  
  # compute variable blocks
  blocks <- new.env()
  b <- 0
  V <- varlist
  while( length(V) >= 1 ){
    b <- b+1
    i <- varblock(V[[1]],V)
    blocks[[paste0('block',b)]] <- unique(unlist(V[i]))
    V <- V[!i]
  }
  blocks <- as.list(blocks)
  
  # logical, indicating rule blocks.
  lapply(blocks,function(b) sapply(varlist,function(v) any(v %in% b) ))
}

# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
  npos <- max(1,ceiling(log10(length(L)+1)))
  fmt <- paste0("%s%0",npos,"d")
  generic <- sprintf(fmt,prefix,seq_along(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given %in% c("", NA)
  given[igen] <- generic[igen]
  make.names(given, unique=T)
}

# Extract variable names from a call object
var_from_call <- function( x, vars=character(0) ){
  
  if ( length(x)==1 && is.symbol(x) ) return(deparse(x) )
  
  if (length(x) > 1){
    for ( i in 2:length(x) ) vars <- c(vars,var_from_call(x[[i]]))
  }
  unique(vars)
}


# r <- ruleset$new(x + y > 10)
# r <- ruleset$new(x + y > 10,z+x > y,a + b == c)
# 
# r$blocks()
# 
# blocks_ruleset(r)
# 
# deparse(r$rules[[1]]@call)












