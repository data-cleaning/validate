#' @include validate_pkg.R
NULL

# File parsing and functions computing on the language

#' Services for extending 'validate'
#'
#' Functions exported silently to allow for cross-package inheritance 
#' of the \code{\link{expressionset}} object. These functions are never
#' needed in scripts or statistical production code.
#'
#' @rdname validate_extend
#' @param .__defaults toggle default options
#' @param .__reset togle reset options 
#' @export
#' @keywords internal
.PKGOPT <- settings::options_manager(
  # all: warnings and errors are raised. 'errors': raise errors. 'none': warnings and errors are caught.
   raise = 'none'
   , lin.eq.eps = 1e-8
   , lin.ineq.eps= 1e-8
   , na.value = NA
   , sequential = TRUE    # option for the 'dcmodify' package
   , na.condition = FALSE # option for the 'dcmodify' package
)


#' Set or get options globally or per object.
#' 
#' 
#' There are three ways to specify options for this package.
#' \itemize{
#' \item{Globally. Setting \code{voptions(option1=value1,option2=value2,...)}
#'   sets global options.}
#' \item{Per object. Setting \code{voptions(x=<object>, option1=value1,...)},
#'   causes all relevant functions that use that object (e.g. 
#'   \code{\link{confront}}) to use those local settings.}
#' \item{At execution time. Relevant functions (e.g. \code{\link{confront}}) take
#'   optional arguments allowing one
#'    to define options to be used during the current function call}
#'  }
#'
#'
#' @section Options for the validate package:
#' Currently the following options are supported.
#' 
#' \itemize{
#'  \item{\code{na.value} (\code{NA},\code{TRUE},\code{FALSE}; \code{NA}) Value
#'  to return when a validating statement results in \code{NA}.}
#'  \item{\code{raise} (\code{"none"},\code{"error"},\code{"all"};
#'  \code{"none"}) Control if the \code{\link{confront}} methods catch or raise
#'  exceptions.
#'  The 'all' setting is useful when debugging validation scripts.}
#'  \item{\code{lin.eq.eps} ('numeric'; 1e-8) The precision used when evaluating
#'  linear equalities. To be used to control for machine rounding.}
#'  \item{\code{"reset"} Reset to factory settings.}
#' }
#'
#'
#' @return When requesting option settings: a \code{list}. When setting options,
#'   the whole options list is returned silently.
#'
#' @param x (optional) an object inheriting from \code{expressionset} such as \code{\link{validator}} or \code{\link{indicator}}.
#' @param ... Name of an option (character) to retrieve options or \code{option = value} pairs to set options. 
#' 
#' 
#' @export
#' @examples
#' # the default allowed validation symbols.
#' voptions('validator_symbols')
#' 
#' # set an option, local to a validator object:
#' v <- validator(x + y > z)
#' voptions(v,raise='all')
#' # check that local option was set:
#' voptions(v,'raise')
#' # check that global options have not changed:
#' voptions('raise')
setGeneric('voptions',def = function(x=NULL,...) standardGeneric('voptions'))

#' @rdname voptions
setMethod('voptions','ANY',function(x=NULL,...){
  do.call(.PKGOPT,c(x,list(...)))
})

#' @rdname voptions
#' @export 
validate_options <- function(...){ 
  .Deprecated(new="voptions")
    voptions(...)
}

#' @rdname voptions
#' @export
setGeneric('reset',def=function(x=NULL) standardGeneric('reset'))




#' @rdname voptions
setMethod('reset','ANY',function(x=NULL){
  settings::reset(.PKGOPT)
})


# get variables from call.
# x : a call object
# output:
# - character vector with variable names
# - NULL         : call contains no variables (e.g. 1 == 1)
var_from_call <- function(x){
  vars <- all.vars(x)

  # Statement containing only literals
  if ( identical(vars, character(0)) ){
    return(NULL)
  } else {
    vars
  }

}

# find a symbol in a call. Returns a list of multi-indices.
# occurrences of variable names in a function signature are skipped.
which.call <- function(x, what, I=1, e=as.environment(list(n=0))){
  # is.symbol filters constants such as NA 
  if ( is.symbol(x) && x == what ){
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


  # 
replace_linear_restriction <- function(x,eps,dat, op="=="){
    repl <- function(x,eps,op){
      # by replacing nodes in the call tree
      # we need not concern about brackets
      if (x[[1]] != op ) return(x)
      m <- expression(e1-e2)[[1]]
      a <- switch(op
        , "==" = expression(abs(x))[[1]]
        , "<=" = expression((x))[[1]]
        , ">=" = expression((x))[[1]] 
      )
      lt <- switch(op
        , "==" =  expression(e1 < e2)[[1]]
        , "<=" = expression(e1 <= e2)[[1]]
        , ">=" = expression(e1 >= e2)[[1]]
      )
      if (op == ">=") eps <- -eps 
      m[[2]] <- left(x)
      m[[3]] <- right(x)
      a[[2]] <- m
      lt[[2]] <- a
      lt[[3]] <- eps
      lt
    }

    if (length(x) == 3 && linear_call(x) && all_numeric(x,dat)){
      return(repl(x,eps,op))
    } else if (length(x) > 1) {
      for ( i in 2:length(x) ){
        x[[i]] <- replace_linear_restriction(x[[i]],eps,dat)
      }
    } 
    x
}
  
all_numeric <- function(x,dat){
  if (is.null(dat)) return(TRUE)
  vr <- var_from_call(x)
  vr <- vr[vr %in% variables(dat)]
  all(sapply(vr,function(u) is.numeric(dat[[u]])))
}
# e <- expression(if (x + y == 3) z>0)[[1]]  
#  e <- expression(aap / noot > z)[[1]]
#  replace_linear_equality(e,1e-8)
  
  
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

# replace occurrences of  'x %in% y' with match(x,y,nomatch=NA,incomparables=NA)
replace_in <- function(x){
  L <- which.call(x,"%in%")
   for ( k in L ){
    m <- expression(e1 %vin% e2)[[1]]
    if (length(k) == 1){
      m[[2]] <- left(x)
      m[[3]] <- right(x)
      x <- m
    } else {
      i <- k[-length(k)]
      e <- x[[i]]
      m[[2]] <- left(e)
      m[[3]] <- right(e)
      x[[i]] <- m
    }
  }
  x
}






# test if a call defines a variable group
defines_var_group <- function(x){
  length(x) == 3 && x[[1]] == ':=' && is.name(x[[2]]) && x[[3]][[1]] == 'var_group'
}



# functions to vectorize validation calls ----
not <- function(x){ 
  f <- expression(!(dummy))[[1]]
  f[[2]][[2]] <- x
  f
}

`%or%` <- function(x,y){
  f <- expression((A)|(B))[[1]]
  f[[2]][[2]] <- x
  f[[3]][[2]] <- y
  f
}
 
# replace_if  <- function(x){
#   f <- expression(!(P) | (Q) )[[1]]
#   f[[c(2,2,2)]] <- x[[2]]
#   f[[c(3,2)]] <- x[[3]]
#   f
# }


# logical implication if (P) Q 
# P -> Q ==> !(P) | (Q)
# 
# if-then-else (length 4)
# if (P) Q else R
# (P -> Q) & (!P -> R)
# 
replace_if  <- function(x){
  if ( length(x) == 3 ){
    f <- expression(!(P) | (Q) )[[1]]
    f[[c(2,2,2)]] <- x[[2]]
    f[[c(3,2)]] <- x[[3]]
    f
  } else { # length (x) == 4 (there is an 'else')
    f <- expression( (!(P) | (Q)) & ((P) | (R)) )[[1]]
    P <- which.call(f,"P")
    f[[P[[1]]]] <- x[[2]]
    f[[P[[2]]]] <- x[[2]]
    Q <- which.call(f,"Q")
    f[[Q[[1]]]] <- x[[3]]
    R <- which.call(f,"R")
    f[[R[[1]]]] <- x[[4]]
    f
  }
}


vectorize <- function(x){
  # we are at an end, or we enter a function, which we will not
  # modify.
  if ( length(x) == 1 || x[[1]] == "function") return(x)
  for ( i in seq_along(x) ){
    if ( is.symbol(x[[i]]) && x[[i]] == "if" ){
      return(vectorize(replace_if(x)))
    } else {
      x[[i]] <- vectorize(x[[i]])
    }
  }
  x
}



# x: a validation call
#vectorize <- function(x) if ( x[[1]] == 'if' ) not(x[[2]]) %or% x[[3]] else  x


# Determine wether a call object represents a linear operation. ----
# the 'length' conditions ensure that for unary operators, the postfix argument is treated as 'right'
node  <- function(x) if ( is.call(x) ) x[[1]] else NULL
left  <- function(x) if ( is.call(x) && length(x)>2) x[[2]] else NULL
right <- function(x) if ( is.call(x) ) x[[min(length(x),3)]] else NULL


linear_call <- function(x){
  if (is.character(x)) return(FALSE)
  if ( is.null(node(x)) ) return(TRUE) 
  n <- deparse(node(x))
  if ( !n %in% c("+","-","*","<","<=","==",">=",">" ) ) return(FALSE)
  if ( n == "*" && !( is.numeric(left(x)) || is.numeric(right(x)) )  ) return(FALSE)
  linear_call(left(x)) & linear_call(right(x))
}



validating_call <- function(cl){
  pure <- c("<", "<=", "==", "!=", ">=", ">", "%in%", "%vin%", "identical", "~" ,"%->%"
          ,"grepl" ,"is_unique", "all_unique","is_complete","all_complete","var_group")
  unary <- c("!", "(", "all", "any" )
  binary <- c("|","||","&","&&","if","xor")

  # push-button semantic changer. See issue #41
  assume_logical = FALSE
  
  vc <- function(x){
    if (is.symbol(x)) return(assume_logical) 
    node <- deparse(x[[1]])
    if ( node %in% pure || grepl("^is\\.",node) ) return(TRUE)
    if ( node %in% unary && vc(x[[2]])) return(TRUE)
    if ( node %in% binary && vc(x[[2]]) && vc(x[[3]]) ) return(TRUE)
    FALSE
  }
  
  vc(cl)
  
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



