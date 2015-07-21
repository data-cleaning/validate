# Martin Morgan's factory; taken from SO. Adapted to make error/warning catching switchable
# Collects results, warnings and errors.
# http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
factory <- function(fun,opts){
  switch(opts('raise')
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

Id <- function(x) x
num_result <- function(x) if (is.list(x)) length(x$result) else length(x)
get_result <- function(x) if (is.list(x)) x$result else x

