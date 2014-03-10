##-------------------------------------------------------------------------
# default symbols allowed to define rules or restrictions
.onLoad <- function(libname,pkgname){
  options(validationSymbols = c(
    '<','<=','==','>','>=', '!=', '%in%', ":"
    , 'identical', 'all','any' 
    , '!', '|', '||', '&', '&&', 'xor'
  ))
}

.onUnload <- function(libpath){
  options(validationSymbols=NULL) 
}



read_resfile <- function(file){
  L <- tryCatch(parse(file=file)
                , error = function(e){
                  cat('Parsing failure at', file,"\n")
                  e
                })
  names(L) <- extract_names(L)
  lapply(L,as.call)
}


# set of restrictions
validator <- setRefClass("validator"
  , contains = 'verifier'
  , methods = list(
    initialize = function(...,files=NULL)  .validator(.self,...,files=files)    
    )
)


.validator <- function(.self, ..., files){
  .verification(.self,...,files=files)
  
  i <- sapply(.self$calls, is.validating)
  if ( !all(i) ){
    warning(paste(
      "The following rules are not validation rules and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  }
  .self$calls <- parse_restrictions(.self$calls[i])
  .self$origin <- .self$origin[i]
  .self
}

parse_restrictions <- function(x){
  x <- lapply(x,extract_datamodel)
  lapply(x,vectorize)
}

is.validating <- function(x, allowed=getOption('validationSymbols'),...){
  sym <- deparse(x[[1]])
  sym %in% allowed || grepl("^is\\.",sym) || ( sym == 'if' && is.validating(x[[2]]) && is.validating(x[[3]]) ) 
}


# Syntactic sugar: translate colon notation 'x : <classname>' to inherits(x,"<classname>")
extract_datamodel <- function(x){
  if ( x[[1]] == ":" ){
    parse(text=paste0("inherits(",deparse(x[[2]]),", '",deparse(x[[3]]),"')"))[[1]]
  } else {
    x
  }
}


# vectorization functions
not <- function(x) parse(text=paste0("!(",deparse(x),")"))[[1]]

`%or%` <- function(x,y){
  parse(text=paste(deparse(x),'|',deparse(y)))[[1]]
} 

vectorize <- function(x){
  if ( x[[1]] == 'if' ){
    not(x[[2]]) %or% x[[3]]
  } else {
    x
  }
}




# example
.onLoad()
r <- validator( z+y>=3)




