## Helper functions, invisible to users.

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


# replace occurences of x$y --> x[,'y']
# for data.frames, this causes an error when selecting a non-existent column.
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

validating <- function(x, allowed=getOption('validationSymbols'),...){
  sym <- deparse(x[[1]])
  sym %in% allowed || grepl("^is\\.",sym) || ( sym == 'if' && validating(x[[2]]) && validating(x[[3]]) ) 
}

# test if a call defines a variable group
vargroup <- function(x){
  length(x) == 3 && x[[1]] == ':' && is.name(x[[2]]) && x[[3]][[1]] == '{'
}

setGeneric("is_vargroup",function(x,...) standardGeneric("is_vargroup"))

setMethod("is_vargroup",signature("verifier"),function(x,...){
  sapply(x$calls, vargroup)  
})


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

# Obtain coefficients of linear rules ----
setGeneric("linear_coefficients",def=function(x,...) standardGeneric("linear_coefficients"))

setMethod("linear_coefficients", signature("validator"),function(x, normalize=TRUE,...){

  calls <- x$calls[is_linear(x)]
  cols <- unique(unlist(lapply(calls, var_from_call)))
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



