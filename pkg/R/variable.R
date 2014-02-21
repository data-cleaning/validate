# The 'indicator' class holds indicator definitions
# An indicator maps a data.frame to a single number.
indicator <- setRefClass("indicator"
  , fields = list(ind='list',origin='character')
  , methods= list(
    initialize = function(...,files=NULL) .indicator(.self, ..., files=files)
    , show = function() .show_ind(.self)
    , compute = function(...) .compute_indicators(.self,...)
    )
)


.indicator <- function(.self,...,files){
  .self$ind <- as.list(substitute(list(...))[-1])
  names(.self$ind) <- extract_names(.self$ind)
}

.show_ind <- function(.self){
  nr <- length(.self$ind)
  cat(sprintf(
    "Reference object of class 'indicator' with %s indicators\n", nr
  ))
  if (nr == 0) return(invisible(NULL))
  lab <- names(.self$ind)
  n <- max(nchar(lab))
  lab <- paste0(format(lab,width=n),": ",sapply(.self$ind,deparse))
  cat(noquote(paste(lab,collapse="\n")))
  cat("\n\n")
}


.compute_indicators <- function(.self,...){
  D <- list(...)
  L <- unlist(lapply(.self$ind,expand,names(D[[1]])))
  sapply(D,function(d) sapply(L,eval,d))
}

# get names from a list, replacing empty names values with numbers
extract_names <- function(L){
  generic <- sprintf("Ind%04d",1:length(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given == ""
  given[igen] <- generic[igen]
  given
}

# a little syntactic sugar to apply functions to multiple variables.
# x : a call object
# vars : character vector
expand <- function(x,vars=NULL){
  M <- x
  vars <- sapply(vars,as.symbol)
  for ( i in 1:length(x))
    if (x[[i]] == '{'){
      if ( length(x[[i]]) > 1 ){ # variables specified as {<x>; <y>; ...}
        M <- lapply(2:length(x[[i]]), function(i) x )
        for ( j in 2:length(x[[i]]) ) M[[j-1]][[i]] <- x[[i]][[j]]
      } else { # all variables
        M <- lapply(seq_along(vars),function(i) x)
        for ( j in seq_along(M) ) M[[j]][[i]] <- vars[[j]]
      }
    }
  M
}



# Example
 data(women)
# specify variables
ind <- indicator(gem=mean({weight; height}, na.rm=TRUE),sd(weight))

ind$compute(w1=women)
ind$compute(w1=women, w2=2*women)

# all variables
ind2 <- indicator(gem=mean({},na.rm=TRUE))
ind2$compute(w=women,w0=2*women)


