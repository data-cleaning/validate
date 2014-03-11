## Helper functions, invisible to users.


is_validating <- function(x, allowed=getOption('validationSymbols'),...){
  sym <- deparse(x[[1]])
  sym %in% allowed || grepl("^is\\.",sym) || ( sym == 'if' && is.validating(x[[2]]) && is.validating(x[[3]]) ) 
}


# functions to vectorize validation calls
not <- function(x) parse(text=paste0("!(",deparse(x),")"))[[1]]

`%or%` <- function(x,y){
  parse(text=paste(deparse(x),'|',deparse(y)))[[1]]
} 

# x: a validation call
vectorize <- function(x) if ( x[[1]] == 'if' ) not(x[[2]]) %or% x[[3]] else  x


# determine wether a call object represents a linear operation.
# the 'length' conditions ensure that for unary operators, the postfix argument is treated as 'right'
node  <- function(x) if ( is.call(x) ) x[[1]] else NULL
left  <- function(x) if ( is.call(x) && length(x)>2) x[[2]] else NULL
right <- function(x) if ( is.call(x) ) x[[min(length(x),3)]] else NULL

# NOTE: x MUST be a call to avoid false positives.
is_linear <- function(x){
  if ( is.null(node(x)) ) return(TRUE) 
  n <- deparse(node(x))
  if ( !n %in% c("+","-","*","<","<=","==",">=",">" ) ) return(FALSE)
  if ( n == "*" && !( is.numeric(left(x)) || is.numeric(right(x)) )  ) return(FALSE)
  is_linear(left(x)) & is_linear(right(x))
}

#  e <- list(
#    e1 = expression(2*x+3*y)[[1]]
#    , e2 = expression(2*x-y*3)[[1]]
#    , e3 = expression(2*x + 3*y - b)[[1]]
#    , e4 = expression(3*x - 2)[[1]]
#    , e5 = expression(3L * x>2)[[1]]
#    , e6 = expression(3*x + y)[[1]]
#    , e7 = expression(-x)[[1]]
#    , e8 = expression(mean(x)+mean(y))[[1]]
#  )
# # # 7 TRUE, 1 FALSE
#  sapply(e,is_linear)

# coefficients for normalized linear expressions (constant after the comparison operator)
nodesign <- c('+' = 1, '-' = -1)
  
coefficients <- function(x, sign=nodesign(deparse(node(x))), coef=new.env()){
  # variable w/o coefficient
  if ( is.name(x) ) { assign(deparse(x),sign,envir=coef); return()}
    
  if ( is.null(node(x)) ) return()

  n <- deparse(node(x))
  
  if (n %in% c("+","-") ) sign <- ifelse(n=='+',1,-1)
  
  if ( n == '*' ){
    val <- if ( is.numeric(left(x)) ) left(x) else right(x)
    var <- if ( is.numeric(left(x)) ) right(x) else left(x)
    assign(deparse(var), sign*val, envir=coef)
  } else {
    coefficients(left(x),1,coef)
    coefficients(right(x),sign,coef)
  }
  unlist(as.list(coef))
}

#lapply(e[1:7],coefficients)
