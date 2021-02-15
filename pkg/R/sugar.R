

#### GROUP DEFINITION SYNTAX --------------------------------------------------

## USER-FACING
# user-defined variable group:
# Returns a function that expands a call in which a group name is used
# to a list of calls.
#
# name  (possibly unqouted) name of the group.
# ...
var_group <- function(...){
  L <- as.list(substitute(list(...))[-1])
  function(){
    if (length(L)==0) return(NULL)
    out <- L[[1]]
    L <<- L[-1]
    out
  }
}

## UNDER THE HOOD


# Replace occurrences of 'vargroup(v1,v2,...,vn)'
# calls: 'list' of calls
# output: the same list of calls, but ocurrences of 'vargroup' have been multiplied
#
expand_groups <- function(calls){
  L <- list()
  for (k in seq_along(calls)){
    # this copies the name.
    U <- calls[k]
    # get reference 
    ref <- get_ref(U)
    # find var groups, if any.
    cl <- calls[[k]]
    I <- which.call(cl,'var_group')
    if (length(I) > 0){
      i <- I[[1]]
      i <- i[-length(i)]
      f <- eval(cl[[i]])
      U <- list()
      while(!is.null(x <- f())){
        u <- cl
        u[[i]] <- x
        U <- c(U,u)
      }
      names(U) <- paste0(names(calls)[k],".",seq_along(U))
      U <- set_ref(U, rep(ref, length(U)))
    }
    L <- c(L,U)
  }
  L <- unlist(L)
  # recurse to check if groups are still present.
  if (length(L) > length(calls)) 
    expand_groups(L) 
  else 
    L
}





#### ASSIGNMENT SUBSTITUTION --------------------------------------------------

## All under the hood

## Substitute assignments in subsequent calls
expand_assignments <- function(calls){
  e <- new.env()
  i <- 1
  lapply(calls, function(x){ 
      x <- substitute_assignments(x,e)
      # add index into original list of calls.
      attr(x,"reference") <- i
      i <<- i+1
      if(x[[1]] == ':=') 
        add_assignment(x,e) 
      x
    }
  )[!is.assignment(calls)]
}

substitute_assignments <- function(call,assignments){
  for ( lhs in ls(assignments) ){
    i <- which.call(call, lhs)
    for ( j in i ){ 
      call[[j]] <- assignments[[lhs]]
    }
  }
  call
}

# add named assignment to environment
# - rhs is 'embraced' for substitution
add_assignment <- function(assignment, e){
  e[[as.character(left(assignment))]] <- right(assignment) 
  assignment
}

# check wether a call is an assignment
is.assignment <- function(x) sapply(x,function(y) y[[1]] == ":=")







