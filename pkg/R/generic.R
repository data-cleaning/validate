
#  Superclass for storing validation rules.
setRefClass("validator"
  , fields = list(calls = 'list',origin= 'character')
  , methods= list(
    initialize = function(...,files=NULL) .validator(.self,...,files=files)
    ,show = function() .show_validator(.self) 
    
    )
)

# confront data with a subclass of 'validator'
setGeneric("confront",
  def = function(x, ...) standardGeneric("confront")
)


# IMPLEMENTATIONS -------------------------------------------------------------

.validator <- function(.self, ..., files){
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
  generic <- sprintf("%04d",1:length(L))
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





