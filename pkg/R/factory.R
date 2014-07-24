# Martin Morgan's factory; taken from SO. Adapted to make error/warning catching switchable
# Collects results, warnings and errors.
# http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
factory <- function(fun,opts){
  switch(opts$getf('raise')
    , 'none' = function(...) { # both errors and warnings are caught
      warn <- err <- NULL
      res <- withCallingHandlers(
        tryCatch(fun(...), error=function(e) {
          err <<- conditionMessage(e)
          NULL
        }), warning=function(w) {
          warn <<- append(warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      list(res, warn=warn, err=err)
    }
    , 'errors' = function(...) { # warnings are caught; errors are raised.
      warn <- err <- NULL
      res <- withCallingHandlers( fun(...)
        , warning=function(w) {
          warn <<- append(warn, conditionMessage(w))
          #invokeRestart("muffleWarning")
        })
      list(res, warn=warn, err=err)
    }
    , 'all' = function(...){
      warn <- err <- NULL
      res <- fun(...) # errors and warinings are raised.
      list(res,warn=warn,err=err)
    }
  )
}

# Martin Morgan's helper-outers
.has <- function(x, what) !sapply(lapply(x, "[[", what), is.null)
hasWarning <- function(x) .has(x, "warn")
hasError <- function(x) .has(x, "err")
isClean <- function(x) !(hasError(x) | hasWarning(x))
value <- function(x) sapply(x, "[[", 1)
cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)
# end

types <- function(x) sapply(x,function(x) class(x[[1]]))
sizes <- function(x) sapply(x,function(x) length(x[[1]]))
Id <- function(x) x
num_result <- function(x) if (is.list(x)) length(x$result) else length(x)
get_result <- function(x) if (is.list(x)) x$result else x

