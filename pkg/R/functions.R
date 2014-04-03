# functions, added to the syntax of validator and indicator objects

#' Count number of missing values
#' 
#' This function should not be called directly, but only be used as syntax when
#' defining an \code{\link{indicator}}.
#' 
#' @param ... comma-separated list of variable names (not character) 
#' @export
number_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- if( length(L) == 0 ) ls() else sapply(L,as.character)
  sum(sapply(
    eapply(
      env=parent.frame()
      , FUN = function(x) sum(is.na(x)) #tryCatch(sum(is.na(x)), warning=function(e) 0)
    )[vars]
  ,Id))
}


# d <- data.frame(
#   x = c(1,NA,3,5)
#   , y = c(NA,NA,1,2)
#   , z = letters[1:4]
#   )
# eval(quote(number_missing(x,y)),envir = d)
