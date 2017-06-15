
setNames <- function(x,nm){
  names(x) <- nm
  x
}

# a reasonable warning
warnf <- function(fmt,...){
  warning(sprintf(fmt,...),call.=FALSE)
}

