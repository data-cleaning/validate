# reference object to store or ignore output
# of validation functions
output <- function(){
  e <- new.env()
  n <- 0            # number of validations
  re <- "^V[0-9]+"  
  e$add <- function(x){
    n <<- n + 1
    e[[sprintf("V%04d",n)]] <- x
  }
  e$gimme <- function(){
    vr <- ls(e,pattern = re)
    lapply(vr, function(i) e[[i]])
  }
  e
}


# retrieve first occurrence of a call from an expression
get_call <- function(expr, call){
  ii <- rev(which.call(expr, call))[[1]]
  if (length(ii)==1) 
    return(expr[[1]])
  else expr[[ ii[-length(ii)] ]]
}

# replace first occurrence of a call in an expression with NULL
rm_call <- function(expr, call){
  ii <- rev(which.call(expr, call))[[1]]
  if ( length(ii) == 1 ){
    expr[[ii]] <- NULL
  } else {
    expr[[ ii[-length(ii)] ]] <- NULL
  }
  expr
}

capture <- function(fun, env, call){
  function(...){
    out <- fun(...)
    # Reconstruct the call that was captured
    out$._call <- get_call(env$expr, call)
    # Remove it from the current expression (since there may
    # be multiple similar calls in one expression)
    env$expr   <- rm_call(env$expr, call)
    # file metadata.
    attr(out, "file")   <- env$file
    attr(out, "lines")  <- c(fst = env$fst, lst=env$lst)
    
    env$add(out)
    out
  }
}

## TODO. Make S4 class out of 'confrontations'
run_validation <- function(file){
  dir <- dirname(file)
  oldwd <- getwd()
  on.exit(setwd(oldwd))
 
  # environment to capture confrontations 
  o <- output()
  o$confront <- capture(confront, o, call="confront")

  fname  <- basename(file)
  parsed <- parse(fname, keep.source=TRUE)
  src    <- attr(parsed, "srcref")

  o$file <- file
  for ( i in seq_along(parsed) ){
    expr  <- parsed[[i]]
    o$fst <- src[[i]][1]
    o$lst <- src[[i]][3]
    o$expr <- expr
    out <- eval(expr, envir=o)
  }
  structure(o$gimme(), class="confrontations")
}


# one-line summary of confrontation
cf_one_liner <- function(x){
  filestr <- attr(x,"file")
  filestr <- if (nchar(filestr)<=16) sprintf("%16s",filestr)
             else paste0("..", substr(attr(x,"file"), 3,16))

  lines <- attr(x,"lines")
  linestr <- sprintf("<%03d:%03d>",lines[1],lines[2])

  callstr <- deparse(x$._call)
  callstr <- gsub(" +", " ", paste(callstr, collapse=" "))
  callstr <- if (nchar(callstr) <= 30) sprintf("%30s",callstr)
             else paste0(substr(callstr,1,28),"..")

  s <- summary(x)
  
  nfpstr <- sprintf("(%02d/%02d|\033[0;32m%02d\033[0m|\033[0;33m%02d\033[0m|\033[0;31m%02d\033[0m)"
                  ,nrow(s), sum(s$items), sum(s$passes),sum(s$nNA) ,sum(s$fails))
  nwestr <- sprintf("[W%02d|E%02d]",sum(s$warning), sum(s$error))

  paste0(filestr, linestr,"|",callstr,"|",nfpstr,nwestr)

}


print.confrontations <- function(x, ...){
   top <- sprintf("Object of class 'confrontations' with %d elements"
      , length(x))
   str <- sapply(x, cf_one_liner)
   cat(top,"\n")
   cat( paste0(sub("^ +"," ",str),collapse="\n"),"\n" )
  
}


summary.confrontations <- function(object,...){
  L <- lapply(object, function(x){ 
      s <- summary(x)
      lines <- attr(x,"lines")
      cbind(file=attr(x,"file"), fst=lines[1], lst=lines[2]
          ,call=deparse(x$._call), s, row.names=NULL)
    })
  out <- do.call("rbind",L)
 # rownames(out) <- NULL
  out
}







