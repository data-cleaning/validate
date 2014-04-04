# functions, added to the syntax of validator and indicator objects

#' Count (fraction of) missing values
#' 
#' This function should not be called directly, but only be used as syntax when
#' defining an \code{\link{indicator}}.
#' 
#' @param ... comma-separated list of variable names (not character). If no
#'  variables are specified, the number of missings over all data is counted.
#'  
#' @return The total number of missings over all specified variables.
#'  
#' @export
number_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- if( length(L) == 0 ) TRUE else sapply(L,as.character)
  sum(sapply(
    eapply(
      env=parent.frame()
      , FUN = function(x) sum(is.na(x)) 
    )[vars]
  ,Id))
}

#' @rdname number_missing
fraction_missing <- function(...){
  L <- as.list(substitute(list(...))[-1])
  vars <- if( length(L) == 0 ) TRUE else sapply(L,as.character)
  v <- sapply(
    eapply(
      env=parent.frame()
      , FUN = function(x) c(sum(is.na(x)),length(x))
    )[vars]
    ,Id)
  sum(v[1,])/sum(v[2,])
}




# d <- data.frame(
#   x = c(1,NA,3,5)
#   , y = c(NA,NA,1,2)
#   , z = letters[1:4]
#   )
# I <- indicator(fraction_missing(x),number_missing(),fraction_missing(x,z))
# 
# values(confront(I,d))
