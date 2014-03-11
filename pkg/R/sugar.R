

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

