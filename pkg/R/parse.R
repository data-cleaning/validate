## Helper functions, invisible to users.


#' Set or get options for the validation package.
#' 
#' @param ... Name of an option (character) to retrieve options or a \code{option = value} pair to set an option. Use 
#' \code{"reset"} to reset the default options.
#' 
#' @section Options for the validate package:
#' Currently the following options are supported.
#' 
#' \itemize{
#'  \item{raise ('none','error','all'; 'none') Control if the \code{\link{confront}} methods catch or raise exceptions. 
#'  The 'all' setting is useful when debugging validation scripts.}
#'  \item{validation_symbols (language; see examples)} Control what statements are allowed as validation statements.
#' }
#' 
#' @examples
#' # the default allowed validation symbols.
#' validate_options('reset')
#' validate_options('validation_symbols')
#' 
#' @export 
validate_options <- function(...){
  L <- list(...)
  if (length(L) == 1 && L[[1]] == 'reset'){
    VOPTION$reset()
    invisible(NULL)
  }
  for ( nm in names(L) )
    VOPTION$set(nm,L[[nm]])
  if ( is.null(nm) ){
    if (length(L) == 0) L = c('raise','validation_symbols') # no arguments given
    setNames(lapply(L,VOPTION$get),L)
  }
}



# class holding options
voption <- setRefClass('voption',
  fields = list(
    validation_symbols = 'character'
    , preproc_symbols = 'character'
    , raise = 'character'
  )
  , methods=list(
    check = function(field){
      if (!field %in% ls(.self)) 
        stop(sprintf('%s is not a valid "voption" field\n',field))      
      
    }
    , set = function(field,value){
      check(field)
      if ( field == 'raise' ) 
        stopifnot( value %in% c('all','none','errors'))
      .self[[field]] <- value
    }
    , get = function(field){
      check(field)
      .self[[field]]
    }
    , reset = function(){
      # top symbols allowed for validation statements
      .self$validation_symbols = c(
        '<','<=','==','>','>=', '!=', '%in%', ":"
        , 'identical', 'all','any', ':=' 
        , '!', '|', '||', '&', '&&', 'xor'
      )
      # all: warnings and errors are raised. 'errors': raise errors. 'none': warnings and errors are caught.
      .self$raise = 'none'
      .self$preproc_symbols = c('<-','library')
    }
  )                  
)

# Create global (hidden for user) option variable
VOPTION <- voption()
VOPTION$reset()



read_resfile <- function(file){
  L <- tryCatch(parse(file=file)
      , error = function(e){
        cat('Parsing failure at', file,"\n")
        e
  })
  # preprocessing execute some statements directly:
  I <- sapply(L,function(x) deparse(x[[1]]) %in% VOPTION$get('preproc_symbols'))
  e <- new.env()
  lapply(L[I],eval,envir=e)
  L <- lapply(L[!I], function(x) do.call(substitute, list(x, env=e)))
  setNames(L,extract_names(L))
}



# find a symbol in a call. Returns a list of multi-indices.
which.call <- function(x, what, I=1, e=as.environment(list(n=0))){
  if (x == what){
    e[[paste0('x',e$n)]] <- I
    e$n <- e$n + 1
  }
  if ( is.call(x) ){    
    for (i in seq_along(x))  which.call(x[[i]],what,c(I,i),e)
  }
  L <- lapply(as.list(e),function(x) if ( length(x) == 1 ) x else x[-1])
  L$n <- NULL
  L
}


# replace occurences x$y --> x[,'y']
replace_dollar <- function(x){
  L <- which.call(x,'$')
  for ( I in L ){
    if (length(I)==1){
      x <- parse(text=paste0(left(x),'[,',deparse(right(x)),']'))[[1]]
    } else {
      I <- I[-length(I)]
      p <- paste0(left(x[[I]]),'[, "',deparse(right(x[[I]])),'" ]')
      x[[I]] <- parse(text=p)[[1]]
    }
  }
  x
}

validating <- function(x,...){
  sym <- deparse(x[[1]])
  sym %in% VOPTION$get("validation_symbols") || 
    grepl("^is\\.",sym) || 
    ( sym == 'if' && validating(x[[2]]) && validating(x[[3]]) ) 
}

# test if a call defines a variable group
vargroup <- function(x){
  length(x) == 3 && x[[1]] == ':' && is.name(x[[2]]) && x[[3]][[1]] == '{'
}



# functions to vectorize validation calls ----
not <- function(x) parse(text=paste0("!(",deparse(x),")"))[[1]]

`%or%` <- function(x,y){
  parse(text=paste(deparse(x),'|',deparse(y)))[[1]]
}

# x: a validation call
vectorize <- function(x) if ( x[[1]] == 'if' ) not(x[[2]]) %or% x[[3]] else  x


# Determine wether a call object represents a linear operation. ----
# the 'length' conditions ensure that for unary operators, the postfix argument is treated as 'right'
node  <- function(x) if ( is.call(x) ) x[[1]] else NULL
left  <- function(x) if ( is.call(x) && length(x)>2) x[[2]] else NULL
right <- function(x) if ( is.call(x) ) x[[min(length(x),3)]] else NULL


linear <- function(x){
  if ( is.null(node(x)) ) return(TRUE) 
  n <- deparse(node(x))
  if ( !n %in% c("+","-","*","<","<=","==",">=",">" ) ) return(FALSE)
  if ( n == "*" && !( is.numeric(left(x)) || is.numeric(right(x)) )  ) return(FALSE)
  linear(left(x)) & linear(right(x))
}



# coefficients for normalized linear expressions (constant after the comparison operator)
nodesign <- c('+' = 1, '-' = -1)
operatorsign <- c('<'= 1, '<=' = 1, '==' = 1, '>=' = -1, '>' = -1)
normed_operators <- c('<' = '<', '<=' = '<=', '==' = '==', '>=' = '<=', '>' = '<')

addcoef <- function(x,value,env) assign(x,mget(x,envir=env,ifnotfound=0)[[1]]+value,env)

# coefficients of an expression of the form sum_i a_i*x_i (so no comparison operators)
coefficients <- function(x, sign=1, coef=new.env()){

  # added constant 
  if ( is.numeric(x) ) addcoef('CONSTANT',sign*x,coef)
  # end node without explicit coefficient.
  if ( is.name(x) ) addcoef(deparse(x),sign,coef)
  
  # wer're at a leaf
  if ( is.null(node(x)) ){ 
    addcoef('CONSTANT',0,coef)
    return(unlist(as.list(coef)))
  }
    
  n <- deparse(node(x))
  if (n %in% c("+","-") ){ 
    sign <- nodesign[n][[1]] # the extra [[1]] gets rid of the 'name' attribute.
  }
  if ( n == '*' ){
    val <- if ( is.numeric(left(x)) ) left(x) else right(x)
    var <- if ( is.name(left(x)) ) left(x) else right(x)
    addcoef(deparse(var), sign*val, coef)
  } else {
    coefficients(left(x),1,coef)
    coefficients(right(x),sign,coef)
  }
  addcoef('CONSTANT',0,coef)
  return(unlist(as.list(coef)))
}



