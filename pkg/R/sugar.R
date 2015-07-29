

#### GROUP DEFINITION SYNTAX --------------------------------------------------

## USER-FACING
# user-defined variable group:
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

## UNDER THE HOOD

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


expand_groups <- function(calls){
  i_groups <- sapply(calls, defines_group)
  if ( !any(i_groups) ) return(calls)
  groups <- setNames(calls[i_groups],NULL)
  calls <- calls[!i_groups]
  unlist( lapply(groups, function(g) lapply(calls, eval(g)) )  )
}


#### ASSIGNMENT SUBSTITUTION --------------------------------------------------

## All under the hood

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







