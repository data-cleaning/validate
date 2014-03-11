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
  .verifier(.self,...,files=files)

  i <- sapply(.self$calls, is_validating)
  if ( !all(i) ){
    warning(paste(
      "The following rules are not validation rules and will be ignored:\n",
      paste(1:sum(!i), ':', sapply(.self$calls[!i],deparse), 'from', .self$origin[!i], collapse="\n ")))
  }
  .self$calls <- lapply(.self$calls[i],vectorize)
  .self$origin <- .self$origin[i]
  .self
}

