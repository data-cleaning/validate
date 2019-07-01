# create reference object to store or ignore output
# of validation functions
output <- function(){
  e <- new.env(parent=globalenv())
  e$n     <- 0 # validating calls
  e$nrule <- 0 # rules executed
  e$nwarn <- 0 # warnings thrown
  e$nerrs <- 0 # errors thrown
  e$nfail <- 0 # items with fails
  e$nNA   <- 0 # items with NA


  re <- "^V[0-9]+"  # regex to retrieve validations
  e$add <- function(x){
    e$n <<- e$n + 1
    e[[sprintf("V%04d",e$n)]] <- x

    # Administration of some of the content
    e$nrule <- e$nrule + length(x)
    e$nwarn <- e$nwarn + confrontation_nwarn(x)
    e$nerrs <- e$nerrs + confrontation_nerrs(x)
    s <- summary(x)
    e$nfail <- e$nfail + sum(s[,"fails"])
    e$nNA   <- e$nNA   + sum(s[,"nNA"])
  }
  e$gimme <- function(){
    vr <- ls(e, pattern = re)
    lapply(vr, function(i) e[[i]])
  }
  e
}


# retrieve first occurrence of a call from an expression
get_call <- function(expr, call){
  ii <- rev(which.call(expr, call))[[1]]
  if ( identical(ii, 1) ){
    return(expr)
  } else if (length(ii)==1) 
    return(expr[[1]])
  else expr[[ ii[-length(ii)] ]]
}

# replace first occurrence of a call in an expression with NULL
rm_call <- function(expr, call){
  ii <- rev(which.call(expr, call))[[1]]
  if ( identical(ii, 1) ){
    return( NULL )
  } else  if ( length(ii) == 1 ){
    expr[[ii]] <- NULL
  } else {
    expr[[ ii[-length(ii)] ]] <- NULL
  }
  expr
}

capture <- function(fun, env){
  function(...){
    out <- fun(...)
    # Reconstruct the call that was captured
    out$._call <- match.call(fun)
    attr(out, "file")   <- env$file
    attr(out, "lines")  <- c(fst = env$fst, lst=env$lst)
    
    env$add(out)
    out
  }
}

## TODO. Make S4 class out of 'confrontations'

#' Run a file with confrontations. Capture results
#'
#' A validation script is a regular R script, intersperced with \code{confront}
#' or \code{check_that} statements. This function will run the script file 
#' and capture all output from calls to these functions.
#'
#' 
#' @param file \code{[character]} location of an R file.
#' @param verbose \code{[logical]} toggle verbose output.
#' 
#' @return an object of class \code{validations}
#'
#' @family validations
#' @export
run_validation_file <- function(file, verbose=TRUE){

  catf <-  function(fmt, ...) if (verbose) cat(sprintf(fmt,...))

  dir <- dirname(file)
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  setwd(dir)  

  # environment to capture confrontations 
  o <- output()
  e <- new.env()
  e$confront   <- capture(confront, o)
  e$check_that <- capture(check_that, o)

  fname  <- basename(file)
  parsed <- parse(fname, keep.source=TRUE)
  src    <- attr(parsed, "srcref")

  prfile <- substr(fname, 1,24)
  prfile <- gsub(" ",".", x=sprintf("%-26s", prfile), fixed=TRUE)


  o$file <- file
  for ( i in seq_along(parsed) ){
    expr  <- parsed[[i]]
    o$fst <- src[[i]][1]
    o$lst <- src[[i]][3]
    o$expr <- expr
    out <- eval(expr, envir=e)
    # print status for quick overview.
    prefix <- sprintf("\rRunning %s %3d calls %3d rules", prfile,o$n, o$nrule)
    postfix <- ""
    if (o$nfail == 0 & o$nNA == 0){ 
      postfix <- "OK"
    } else { 
      if (o$nfail > 0) postfix <- sprintf("%3d FAILS", o$nfail)
      if (o$nNA   > 0) postfix <- sprintf("%s %3d NA", postfix, o$nNA)
      if (o$nerrs > 0) postfix <- sprintf("%s %3d ERRORS",postfix, o$nerrs)
      if (o$nwarn > 0) postfix <- sprintf("%s %3d WARNINGS",postfix, o$nwarn)
    }
    catf("%s %s",prefix,postfix)
  }
  catf("\n")
  structure(o$gimme(), class="validations")
}


# one-line summary of confrontation
## TODO: width of nr of items should adapt to nr of actual items.
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

cf_legend <- function(){
"Legend: file.R<line_start:line_end>|call|(rules/items)[\033[0;32mPASSES\033[0m|\033[0;33mMISSING\033[0m|\033[0;31mFAILS\033[0m]"
}

#' print a 'confrontations' object
#'
#' @param x \code{[confrontations]} object
#' @param ... currently unused
#'
#' @family validations 
#' @export
print.validations <- function(x, ...){
   top <- sprintf("Object of class 'validations' with %d elements"
      , length(x))
   str <- sapply(x, cf_one_liner)
   cat(top,"\n")
   cat( paste0(sub("^ +"," ",str),collapse="\n"),"\n" )
   cat(cf_legend(),"\n")
}

#' Summarize a 'validations' object
#'
#' @param object An object of class \code{confrontations}
#' @param ... currently unused
#'
#' @family validations
#' @export
summary.validations <- function(object,...){
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







