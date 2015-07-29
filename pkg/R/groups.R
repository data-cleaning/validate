


# replace a name by a call in a 'call' object.
# x : a call object
# what: a character, name to replace
# replacement: a call to replace the name with.
replace_in <- function(x, what, replacement){
  if ( x == what ){
    return (replacement)
  } else if ( length(x) > 1 ) {
    for ( i in seq_along(x) ) 
      x[[i]] <- replace_in(x[[i]],what,replacement)
  }
  x
}

defines_group <- function(x){
  x[[1]] == "var_group"
}


# user-defined variable group
# Returns a function that expands a call in which a group name is used
# to a list of calls.
#
# name  (possibly unqouted) name of the group.
# ...
var_group <- function(name,...){
  group <- as.character(substitute(name))
  groupvars <- as.list(substitute(list(...))[-1])
  
  function(cl){
    if ( !group %in% all.vars(cl) ) return(cl)
    lapply(groupvars,function(v){
      replace_in(cl,group,v)
    })
  }
}


expand_groups <- function(calls){
  i_groups <- sapply(calls, defines_group)
  if ( !any(i_groups) ) return(calls)
  groups <- setNames(calls[i_groups],NULL)
  calls <- calls[!i_groups]
  unlist( lapply(groups, function(g) lapply(calls, eval(g)) )  )
}



# # demonstruction
# m <- group(name=groupy, x, y)
# e <- expression(groupy > 0)[[1]]
# m(e)
L <- list(
  V1 = expression(var_group(g,x,y))[[1]]
  , V2 = expression(g > 0)[[1]]
  , V3 = expression(x + y == z)[[1]]
)
K <- expand_groups(L)
K
# 
# L <- list(
#   V1 = expression(var_group(name=G, ))[[1]]
#   , V2 = expression(G > 0)[[1]]
# )
# 
# expand_groups(L,varlist=c("X1","Xmoo","YXp"))
# 
# 
# v <- validator(var_group(name=G,x,y,"X.*"),G>0)
# dat <- data.frame(x = runif(10)-1/2,y=runif(10)-1/2, X10 = rnorm(10))
# confront(dat,v)

# L <- list(
#   expression(x + y ==2)[[1]]
#   , expression(x-y == 2)[[1]]
# )
# expand_groups(L)





