# functions, added to the syntax of validator and indicator objects

number_na <- function(){
  sum(sapply(
    eapply(function(x)  
      tryCatch(sum(is.na(x)), warning=function(e) 0)
    , env=parent.frame())
  ,Id))
}

