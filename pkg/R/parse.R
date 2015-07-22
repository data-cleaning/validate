#' @include validate_pkg.R
NULL

# File parsing and functions computing on the language



PKGOPT <- options_manager(
  validator_symbols = c(
    '<','<=','==','>','>=', '!=', '%in%', "~", "%->%"
    , 'identical', 'all','any', ':=' 
    , '!', '|', '||', '&', '&&', 'xor'
    , 'any_duplicated', 'any_missing'
  )
  # all: warnings and errors are raised. 'errors': raise errors. 'none': warnings and errors are caught.
  , raise = 'none'
  , preproc_symbols = c('<-','library')
)


#' Set or get options globally or per object.
#' 
#' @section Options for the validate package:
#' Currently the following options are supported.
#' 
#' \itemize{
#'  \item{raise ('none','error','all'; 'none') Control if the \code{\link{confront}} methods catch or raise exceptions. 
#'  The 'all' setting is useful when debugging validation scripts.}
#'  \item{validator_symbols (language; see examples)} Control what statements are allowed as validation statements.
#'  \item{'reset'} Reset to factory settings.
#' }
#'
#'
#' @section Details:
#' There are three ways in which options can be specified.
#' \itemize{
#' \item{Globally. Setting \code{validate_options(option1=value1,option2=value2,...)} sets global options.
#' \item{Per object. Setting \code{validate_options(where=<object>, option1=value1,...)}, causes all relevant functions
#' that use that object (e.g. \code{\link{confront}}) to use those local settings.}
#' \item{At execution time. Relevant functions (e.g. \code{\link{confront}}) take optional arguments allowing one
#' to define options to be used during the current function call}
#' }}
#' 
#' To set options in a file, use \code{validate_options(option1=value1,option2=value2,...)} without the \code{where}
#' argument. This will invoke a local setting in the object created when the file is parsed.
#' 
#' @return When requesting option settings: a \code{list}. When setting options, the whole options 
#' list is returned silently.
#'
#' @param x (optional) an object inheriting from \code{expressionset} such as \code{\link{validator}} or \code{\link{indicator}}.
#' @param ... Name of an option (character) to retrieve options or \code{option = value} pairs to set options. 
#' 
#' 
#' @export
#' @examples
#' # the default allowed validation symbols.
#' validate_options('validator_symbols')
#' 
#' # set an option, local to a validator object:
#' v <- validator(x + y > z)
#' validate_options(v,raise='all')
#' # check that local option was set:
#' validate_options(v,'raise')
#' # check that global options have not changed:
#' validate_options('raise')
setGeneric('validate_options',def = function(x=NULL,...) standardGeneric('validate_options'))

#' @rdname validate_options
setMethod('validate_options','ANY',function(x=NULL,...){
  do.call(PKGOPT,c(x,list(...)))
})

#' @rdname validate_options
#' @export
setGeneric('validate_reset',def=function(x=NULL) standardGeneric('validate_reset'))

#' @rdname validate_options
setMethod('validate_reset','ANY',function(x=NULL){
  settings::reset(PKGOPT)
})




# Extract variable names from a call object
var_from_call <- function( x, vars=character(0) ){
  
  if ( length(x)==1 && is.symbol(x) ) return(deparse(x) )
  
  if (length(x) > 1){
    for ( i in 2:length(x) ) vars <- c(vars,var_from_call(x[[i]]))
  }
  unique(vars)
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


# test if a call defines a variable group
defines_var_group <- function(x){
  length(x) == 3 && x[[1]] == ':=' && is.name(x[[2]]) && x[[3]][[1]] == 'var_group'
}



# functions to vectorize validation calls ----
not <- function(x) parse(text=paste0("!(",deparse(x),")"))[[1]]

`%or%` <- function(x,y){
  parse(text=paste(call2text(x),'|',call2text(y)))[[1]]
}

# x: a validation call
vectorize <- function(x) if ( x[[1]] == 'if' ) not(x[[2]]) %or% x[[3]] else  x


# Determine wether a call object represents a linear operation. ----
# the 'length' conditions ensure that for unary operators, the postfix argument is treated as 'right'
node  <- function(x) if ( is.call(x) ) x[[1]] else NULL
left  <- function(x) if ( is.call(x) && length(x)>2) x[[2]] else NULL
right <- function(x) if ( is.call(x) ) x[[min(length(x),3)]] else NULL


linear_call <- function(x){
  if ( is.null(node(x)) ) return(TRUE) 
  n <- deparse(node(x))
  if ( !n %in% c("+","-","*","<","<=","==",">=",">" ) ) return(FALSE)
  if ( n == "*" && !( is.numeric(left(x)) || is.numeric(right(x)) )  ) return(FALSE)
  linear_call(left(x)) & linear_call(right(x))
}

# check whether a call is validating, based on a set of 
# predifined allowed symbols that result in a boolean.
validating_call <- function(call,allowed_symbols){
  sym <- deparse(call[[1]])
  sym %in% allowed_symbols || 
    grepl("^is\\.",sym) || 
    ( sym == 'if' && validating_call(call[[2]],allowed_symbols) && 
        validating_call(call[[3]],allowed_symbols) ) 
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



