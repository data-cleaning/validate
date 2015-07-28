


# replace a name by a call in a 'call' object.
# x : a call object
# what: a character, name to replace
# replacement: a call to replace the name with.
replace_in <- function(x, what, replacement){
  if ( x == what ){
    return (replacement)
  } else if ( length(x)>1 ) {
    for ( i in seq_along(x) ) 
      x[[i]] <- replace_in(x[[i]],what,replacement)
  }
  x
}

defines_group <- function(x){
  x[[1]] == "group"
}


group <- function(name,...){
  group <- deparse(substitute(name))
  groupvars <- substitute(list(...))[-1]
  
  
  function(cl){
    lapply(groupvars,function(v){
      replace_in(cl,group,v)
    })
  }
}




# # demonstruction
# m <- group(name=groupy, x, y)
# e <- expression(groupy > 0)[[1]]
# m(e)










