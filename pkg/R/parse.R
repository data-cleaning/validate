## Helper functions, invisible to users.

parse_restrictions <- function(x){
  #  x <- lapply(x,extract_datamodel)
  lapply(x,vectorize)
}

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
vectorize <- function(x){
  if ( x[[1]] == 'if' ){
    not(x[[2]]) %or% x[[3]]
  } else {
    x
  }
}


# determine wether a call object represents a linear operation.
node  <- function(x) if ( is.call(x) ) x[[1]] else NULL
left  <- function(x) if ( is.call(x) ) x[[2]] else NULL
right <- function(x) if ( is.call(x) ) x[[3]] else NULL

# NOTE: x MUST be a call to avoid false positives.
is_linear <- function(x){
  if ( is.null(node(x)) ) return(TRUE) 
  n <- deparse(node(x))
  if ( !n %in% c("+","-","*" ) ) return(FALSE)
  if ( n == "*" && !( inherits(left(x), 'numeric') || inherits(right(x),"numeric") )  ) return(FALSE)
  is_linear(left(x)) & is_linear(right(x))
}

# e <- list(
#   e1 = expression(2*x+3*y)[[1]]
#   , e2 = expression(2*x-y*3)[[1]]
#   , e3 = expression(2*x + 3*y - b)[[1]]
#   , e4 = expression(3*x - 2)[[1]]
#   , e5 = expression(3*x)[[1]]
#   , e6 = expression(mean(x)+mean(y))[[1]]
# )
# # 5 TRUE, 1 FALSE
# sapply(e,is_linear)
























# Extract user-defined names from an object returned by 'parse'
#  extract_names <- function(L,labeltext='label'){
#   labeltext <- paste0('@',labeltext)
#   
#   i <- getSrcLocation(L)
#   d <- getParseData(L)
#   
#   names <- paste0(rep("000",length(L)),seq_along(L))
#   names <- paste0("R",substring(names,nchar(names)-2))
#   if ( !is.null(names(L)) ){
#     iname <- names(L) != ""
#     names[iname] <- names(L)[iname]
#   }
#   # select comments
#   d <- d[d$token == "COMMENT",c('line1','text')]
#   # select name definitions
#   d <- d[grep(labeltext,d$text), ]
#   srcloc <- d$line1 + 1
#   
#   srclab <- gsub(paste0("^.+",labeltext," "),"",d$text)
#   srclab <- gsub("\\s+.+$","",srclab)
#   
#   # merge names and definitions
#   j <- match(i,srcloc,nomatch=0)
#   names[which(i %in% srcloc)] <- srclab[j]
#   names  
# }





