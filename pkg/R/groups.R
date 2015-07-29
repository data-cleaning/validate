


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







