#' @import methods
NULL


#' Set or get options globally or per object.
#' 
#' @param where (optional) an object inheriting from \code{expressionset}, like \code{\link{validator}} or \code{\link{indicator}}.
#' @param ... Name of an option (character) to retrieve options or \code{option = value} pairs to set options. Use 
#' \code{"reset"} to reset the default options. There is a special option \code{addsymbol} that allows for registring
#' extra validation symbols it's value should be \code{character}.
#' 
#' @section Options for the validate package:
#' Currently the following options are supported.
#' 
#' \itemize{
#'  \item{raise ('none','error','all'; 'none') Control if the \code{\link{confront}} methods catch or raise exceptions. 
#'  The 'all' setting is useful when debugging validation scripts.}
#'  \item{validation_symbols (language; see examples)} Control what statements are allowed as validation statements.
#'  \item{'reset'} Reset to factory settings.
#' }
#' 
#' @section Details:
#' There are three ways in which options can be specified.
#' \itemize{
#' \item{Globally. Setting \code{validate_options(option1=value1,option2=value2,...)} causes all relevant commands 
#' (e.g. \code{\link{confront}}, \code{\link{validator}}) to use the new options from then on.}
#' \item{Per object. Setting \code{validate_options(option1=value1,...,where=<object>)}, causes all relevant functions
#' that use that object (e.g. \code{\link{confront}}) to use those local settings.}
#' \item{At execution time. Relevant functions (e.g. \code{\link{confront}}) take optional arguments allowing one
#' to define options to be used during the current function call}
#' }
#' 
#' To set options in a file, use \code{validate_options(option1=value1,option2=value2,...)} without the \code{where}
#' argument. This will invoke a local setting in the \code{\link{validator}} or \code{\link{indicator}} object when
#' the file is read.
#' 
#' @return When requesting option settings: a \code{list}. When setting options, the whole options 
#' list is returned silently.
#' 
#' 
#' @examples
#' # the default allowed validation symbols.
#' validate_options('reset')
#' validate_options('validation_symbols')
#' 
#' # set an option, local to a validator object:
#' v <- validator(x + y > z)
#' validate_options(raise='all', where=v)
#' # check that local option was set:
#' validate_options('raise',where=v)
#' # check that global options have not changed:
#' validate_options('raise')
#' 
#' @export 
validate_options <- function(...,where=NULL){
  stopifnot( is.null(where) || inherits(where, 'expressionset') )
  if ( !is.null(where) ){ 
    where$options(...)
  } else {
    v_option(VOPTION,...)
  }  
}


# x a 'voption' object
# ... name=value pairs for options or just a name
# copy create a copy of x and return?
# @return If ... is a name, a single-item list with the option.
#         If ... is name=value pairs, either x, or an altered copy of it, silently.
v_option <- function(x,...,addsymbols=NULL,copy=FALSE){
  L <- list(...)
  
  # how may arguments?
  nargs <- length(L)
  # are we getting (T) or setting (F) options?
  getmod <- is.null(names(L))
  setmod <- !getmod
  
  if ( nargs == 0 &&  copy ) return(x$copy())
  if ( nargs == 0 && !copy ) return(as.list(x))

  if ( nargs == 1 && is.character(L[[1]]) && L[[1]] == 'reset' && !copy ){
    x$reset()
    return(invisible(as.list(x)))
  }
  
  if ( nargs > 0 && getmod && copy ){
    stop('Copy not possible when requesting fields explicitly')
  }
  
  if ( nargs > 0 && getmod && !copy){
    # return a list of requested options.
    return( setNames( lapply(L, function(field) x$getf(field)), L ))
  }
  
  appendsymb <- function(OPT,LL){
    i <- match("addsymbol",names(LL))
    if ( !is.na(i) ){
      OPT$setf("validation_symbols",append(OPT$getf("validation_symbols"),LL[[i]]))
      message(sprintf("Registred symbol(s): %s",paste(L[[1]],collapse=", ")))
      LL <- LL[-i]
    }
    LL
  }
  if ( nargs > 0 && setmod && copy){
    opt <- x$copy()
    L <- appendsymb(opt,L)
    for ( nm in names(L) ) opt$setf(nm,L[[nm]])
    return(opt)
  }
  
  if ( nargs > 0 && setmod && !copy){
    L <- appendsymb(x,L)
    for ( nm in names(L) ) x$setf(nm,L[[nm]])
    return(invisible(as.list(x)))
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
    , setf = function(field,value){
      check(field)
      if ( field == 'raise' ) 
        stopifnot( value %in% c('all','none','errors'))
      .self[[field]] <- value
    }
    , getf = function(field){
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

setGeneric('as.list')
setMethod('as.list',signature('voption'),function(x,...){
  fnames <- c('raise','validation_symbols','preproc_symbols')
  setNames(lapply(fnames,function(i) x$getf(i)),fnames)
})


# copy option object and alter with named arguments in ...
copy_and_set <- function(x,...){
  L <- list()
  y <- x$copy()
  for ( nm in names(L) ) y$setf(nm,L[[nm]])
  y
}

# Create global (hidden for user) option variable
VOPTION <- voption()
VOPTION$reset()

parse_annotations <- function(L){
  line_nums <- sapply(attr(L, "srcref"), function(sr){sr[1]})
  parseData <- getParseData(L)
  parseData <- subset(parseData, token=="COMMENT")
  
  # filter out @name
  NAME <- "#\\s+@name\\s+([^\\s]+).*"
  name_lines <- parseData[grepl(NAME, parseData$text),]
  name_lines$name <- sub(NAME, "\\1", name_lines$text)
  name_lines$match <- match(name_lines$line2+1, line_nums)
  names(L)[name_lines$match] <- name_lines$name
  #print(list(line_nums=line_nums, parseData=parseData, name_lines=name_lines, names_L = names(L)))
  L
}

# x: an object of class expressionset
read_resfile <- function(file, x){
  L <- tryCatch(parse(file=file, keep.source = T)
      , error = function(e){
        cat('Parsing failure at', file,"\n")
        e
  })
  L <- parse_annotations(L)
  # set options in x (if any)
  I <- sapply(L, function(x) deparse(x[[1]]) == 'validate_options')
  if ( any(I) ){
    e <- new.env()
    e$x <- x
    M <- lapply(L[I],deparse)
    v <- lapply(M, function(x) parse(text=gsub(")$",", where = x)",x))[[1]])
    lapply(v,eval,e)
    L <- L[!I]
  }
  # preprocessing execute some statements directly:
  I <- sapply(L,function(y) deparse(y[[1]]) %in% x$options('preproc_symbols')[[1]])
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

validating <- function(x,y,...){
  sym <- deparse(x[[1]])
  sym %in% y$options("validation_symbols")[[1]] || 
    grepl("^is\\.",sym) || 
    ( sym == 'if' && validating(x[[2]],y) && validating(x[[3]],y) ) 
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



