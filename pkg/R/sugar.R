
# Variable groups can be defined as <groupname> : {<var1>; <var2>; ... ; <varN> }
#
# This function expands a variable group and stores it 
# as character array in an environment. Input MUST be valid vargroup definition call
expand_vargroup <- function(x){  
  e <- new.env()
  
  addgroup <- function(xi){
    group <- as.character(left(xi))
    
    if ( !is.null(e[[group]]) ) warning(sprintf('Multiply defined group %s\n',group))
    
    vars <- sapply(right(xi),as.character)[-1]
    
    if ( length(vars) == 0 ){ 
      warning(sprintf('Ignoring empty group %s\n',group))
    }
    
    e[[group]] <- vars
    
    fail <- ls(e) %in% vars
    if ( any(fail) ){
      stop(sprintf("Group '%s' also occurs as variable\n",ls(e)[fail]))  
    }
  }
  
  lapply(x,addgroup)
  as.list(e)
}

# determine which groups a call refers to.
groups_from_call <- function(x, groups, e=new.env()){
  if ( is.name(x) && deparse(x) %in% groups ) assign(as.character(x),1,envir=e)
  for (i in seq_along(x)[-1]) groups_from_call(x[[i]],groups,e)
  return(ls(e))
}

has_group <- function(x, group, has=FALSE){
  if (is.name(x) && deparse(x) == group ) return(TRUE)
  for(i in seq_along(x)[-1] ) has = has | has_group(x[[i]],group)
  has
}


expand_group <- function(calls, group, variables){
  e <- new.env()
  L <- lapply(calls,function(call){
    if ( !has_group(call,group) ) return(list(call))
    setNames( lapply(variables, function(x) {
        assign(group, as.symbol(x), envir=e)
        do.call(substitute,list(call, e))
      })
      , variables
    )
  })
#  origin <- rep(names(L),times=sapply(L,length))
#  L <- unlist(L,use.names=FALSE)
  
#  names(L) <- paste(origin,variables,sep=".")
  unlist(L)
}

expand_groups <- function(calls, groups){
  igroup <- sapply(calls, vargroup)
  
  groups <- expand_vargroup(calls[igroup])

  calls <- calls[!igroup]
  for ( group in names(groups) ){
    calls <- expand_group(calls,group,groups[[group]])
  }
  calls  
}


## Substitute assignments in subsequent calls
expand_assignments <- function(calls){
  e <- new.env()
  lapply(calls, function(x) 
    if(x[[1]] == ':=') 
      add_assignment(x,e) 
    else 
      substitute_assignments(x,e)
    )[!is.assignment(calls)]
}

substitute_assignments <- function(call,assignments){
  for ( lhs in ls(assignments) ){
    i <- which.call(call,lhs)
    for ( j in i){ 
      call[[j]] <- assignments[[lhs]]
    }
  }
  call
}

# add named assignment to environment
# - rhs is 'embraced' for substitution
add_assignment <- function(assignment, e){
  rhs <- expression((.))[[1]]
  rhs[[2]] <- right(assignment)
  e[[as.character(left(assignment))]] <- rhs
  assignment
}

# check wether a call is an assignment
is.assignment <- function(x) sapply(x,function(y) y[[1]] == ":=")




# which_groups <- function(x, groups, I=1, e=new.env()){
#   if ( is.name(x) && deparse(x) %in% groups ){
#     g <- deparse(x)
#     if ( is.null(e[[g]]) ){ 
#       e[[g]] <- list(I)
#     } else {
#       e[[g]] <- c(e[[g]],I)
#     }
#   }
#   for (i in seq_along(x)[-1]) which_groups(x[[i]],grp,c(I,i),e)
#   lapply(as.list(e),function(x) if ( length(x) == 1 ) x else x[-1])
# }
# 
# # x: a call
# expand_calls <- function(x,groups){
#   g <- names(groups)
#   I <- which_groups(x,groups)
#   
# }
#   
  
  

## this stuff is experimental syntactic sugar.
# .expand <- function(.self,vars){
#   L <- lapply(.self$calls,expand,vars)
#   .self$origin <- unlist(lapply(seq_along(L), function(i) rep(.self$origin[i],length(L[[i]]))))
#   .self$calls <- unlist(L)
# }



## OBSOLETE ----

# # a little syntactic sugar to apply functions to multiple variables.
# # x : a call object
# # vars : character vector
# expand <- function(x,vars=NULL){
#   M <- x
#   if (!is.null(vars)) vars <- sapply(vars,as.symbol)
#   for ( i in 1:length(x))
#     if (x[[i]] == '{'){
#       if ( length(x[[i]]) > 1 ){ # variables specified as {<x>; <y>; ...}
#         M <- lapply(2:length(x[[i]]), function(i) x )
#         for ( j in 2:length(x[[i]]) ) M[[j-1]][[i]] <- x[[i]][[j]]
#       } else { # all variables
#         if (is.null(vars)) stop('You need to provide variable names for indicators with unspecified variables')
#         M <- lapply(seq_along(vars),function(i) x)
#         for ( j in seq_along(M) ) M[[j]][[i]] <- vars[[j]]
#       }
#     }
#   M
# }





# expand group, simple deparse/parse version
# expand_call <- function(x,e){
#   x <- gsub("[ ]+","",paste0(deparse(x),collapse=""))
#   groups <- ls(e)[[2]]
#   # todo: loop over multiple groups
#   X <- lapply(e[[groups]], function(var) gsub(paste0("{",groups,"}"),var,x,fixed=TRUE))
#   lapply(X,function(m) parse(text=m)[[1]])
# }



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




