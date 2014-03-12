## Helper functions, invisible to users.


is_validating <- function(x, allowed=getOption('validationSymbols'),...){
  sym <- deparse(x[[1]])
  sym %in% allowed || grepl("^is\\.",sym) || ( sym == 'if' && is_validating(x[[2]]) && is_validating(x[[3]]) ) 
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
setGeneric("is_linear", def=function(x,...) standardGeneric("is_linear"))


setMethod("is_linear",signature("validator"), function(x,...){
  sapply(x$calls, linear)
})


linear <- function(x){
  if ( is.null(node(x)) ) return(TRUE) 
  n <- deparse(node(x))
  if ( !n %in% c("+","-","*","<","<=","==",">=",">" ) ) return(FALSE)
  if ( n == "*" && !( is.numeric(left(x)) || is.numeric(right(x)) )  ) return(FALSE)
  linear(left(x)) & linear(right(x))
}

#  e <- list(
#    e1 = expression(2*x+3*y)[[1]]
#    , e2 = expression(2*x-y*3)[[1]]
#    , e3 = expression(2*x + 3*y - b)[[1]]
#    , e4 = expression(3*x - 2)[[1]]
#    , e5 = expression(3L * x)[[1]]
#    , e6 = expression(3*x + y)[[1]]
#    , e7 = expression(-x)[[1]]
#    , e8 = expression(-y + 2 - 4)[[1]]
#    , e9 = expression(mean(x)+mean(y))[[1]]
#  )
# # # 7 TRUE, 1 FALSE
#  sapply(e,is_linear)


setGeneric("linear_coefficients",def=function(x,...) standardGeneric("linear_coefficients"))

setMethod("linear_coefficients", signature("validator"),function(x, normalize=TRUE,...){

  calls <- x$calls[is_linear(x)]
  cols <- sapply(calls, var_from_call)
  rows <- names(calls)
  
  bA <- matrix(0
   , nrow = length(rows)
   , ncol = length(cols) + 1
   , dimnames = list(validator=rows, variable=c('CONSTANT',cols) )
  )
  
  lcoef <- lapply(calls, function(x) coefficients(left(x)))
  rcoef <- lapply(calls, function(x) coefficients(right(x)))
  
  for ( i in seq_along(lcoef) ){
    cls <- names(lcoef[[i]])
    bA[i,cls] <- lcoef[[i]]
    cls <- names(rcoef[[i]])
    bA[i,cls] <- bA[i,cls] - rcoef[[i]]
  }
  
  operators <- sapply(sapply(calls,`[[`,1),deparse)
  
  if (normalize){
    bA <- bA * operatorsign[operators]
    operators <- normed_operators[operators]
  }
  
  list(A=bA[,-1,drop=FALSE],b = -1*bA[,1,drop=FALSE],operators=operators)
  
})


# coefficients for normalized linear expressions (constant after the comparison operator)
nodesign <- c('+' = 1, '-' = -1)
operatorsign <- c('<'= 1, '<=' = 1, '==' = 1, '>=' = -1, '>' = -1)
normed_operators <- c('<' = '<', '<=' = '<=', '==' = '==', '>=' = '<=', '>' = '<')

addcoef <- function(x,value,env) assign(x,mget(x,envir=env,ifnotfound=0)[[1]]+value,env)

# coefficients of an expression of the form sum_i a_i*x_i (so no comparison operators)
coefficients <- function(x, sign=1, coef=new.env()){
  # variable w/o coefficient
  if ( is.numeric(x) ) addcoef('CONSTANT',x,coef)
  if ( is.name(x) ) addcoef(deparse(x),sign,coef) 

  if ( is.null(node(x)) ){ 
    addcoef('CONSTANT',0,coef)
    return(unlist(as.list(coef)))
  }
  
  n <- deparse(node(x))
  if (n %in% c("+","-") ){ 
    sign <- nodesign[n][[1]] # the extra [[1]] gets rid of the 'name' attribute.
    if (is.numeric(left(x))){
      coef$CONSTANT <- ifelse(is.numeric(coef$CONSTANT),coef$CONSTANT + sign*left(x),left(x))
    }
    if (is.numeric(right(x))){
      coef$CONSTANT <- ifelse(is.numeric(coef$CONSTANT),coef$CONSTANT + sign*right(x),sign*right(x))      
    }
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

# get top operator from validating call
validation_operator <- function(x) x[[1]]






#lapply(e[1:7],coefficients)
