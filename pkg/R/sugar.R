

# syntactic sugar that may or may not make it to the package

# Syntactic sugar: translate colon notation 'x : <classname>' to inherits(x,"<classname>")
extract_datamodel <- function(x){
  if ( x[[1]] == ":" ){
    parse(text=paste0("inherits(",deparse(x[[2]]),", '",deparse(x[[3]]),"')"))[[1]]
  } else {
    x
  }
}


## this stuff is experimental syntactic sugar.
.expand <- function(.self,vars){
  L <- lapply(.self$calls,expand,vars)
  .self$origin <- unlist(lapply(seq_along(L), function(i) rep(.self$origin[i],length(L[[i]]))))
  .self$calls <- unlist(L)
}


# a little syntactic sugar to apply functions to multiple variables.
# x : a call object
# vars : character vector
expand <- function(x,vars=NULL){
  M <- x
  if (!is.null(vars)) vars <- sapply(vars,as.symbol)
  for ( i in 1:length(x))
    if (x[[i]] == '{'){
      if ( length(x[[i]]) > 1 ){ # variables specified as {<x>; <y>; ...}
        M <- lapply(2:length(x[[i]]), function(i) x )
        for ( j in 2:length(x[[i]]) ) M[[j-1]][[i]] <- x[[i]][[j]]
      } else { # all variables
        if (is.null(vars)) stop('You need to provide variable names for indicators with unspecified variables')
        M <- lapply(seq_along(vars),function(i) x)
        for ( j in seq_along(M) ) M[[j]][[i]] <- vars[[j]]
      }
    }
  M
}


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




