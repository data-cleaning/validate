
#' Logging object to use with the lumberjack package
#'
#' @section Details: 
#'
#' This obeject can used with the function composition ('pipe') operator of the 
#' \code{\link[lumberjack]{lumberjack}} package. The logging is based on
#' validate's \code{\link{cells}} function. The output is written to a
#' \code{csv} file wich contains the following columns.
#' \tabular{lll}{
#' \code{step}           \tab\code{integer}  \tab Step number  \cr
#' \code{time}           \tab\code{POSIXct}  \tab Timestamp    \cr
#' \code{expr}           \tab\code{character}\tab Expression used on data \cr
#' \code{cells}          \tab\code{integer}  \tab Total nr of cells in dataset\cr
#' \code{available}      \tab\code{integer}  \tab Nr of non-NA cells\cr
#' \code{missing}        \tab\code{integer}  \tab Nr of empty (NA) cells\cr
#' \code{still_available}\tab\code{integer}  \tab Nr of cells still available after expr\cr
#' \code{unadapted}      \tab\code{integer}  \tab Nr of cells still available and unaltered\cr
#' \code{unadapted}      \tab\code{integer}  \tab Nr of cells still available and altered\cr
#' \code{imputed}        \tab\code{integer}  \tab Nr of cells not missing anymore\cr
#' }
#' 
#'
#' @section Note:
#' This logger is suited only for operations that do not change the dimensions 
#' of the dataset.
#'
#' @docType class
#' @format A reference class object
#' @family loggers
#'
#' @export lbj_cells
#' @exportClass lbj_cells
lbj_cells <- setRefClass("lbj_cells"
    , fields = list(
        cells = "array"
        , n     = "numeric"
        , t     = "POSIXct"
        , expr  = "character"
        , verbose = "logical"
        , label = "character"
        )
    , methods = list(
      initialize = function(..., verbose=TRUE, label=""){
        "Create object. Optionally toggle verbosity."
        .self$n <- 0
        .self$t <- .POSIXct(numeric(0))
        .self$verbose = verbose
        .self$label = label
      }
      , add = function(meta, input, output){
        "Add logging info based on in- and output"
        if (!identical(dim(input),dim(output))){
          warnf("dimensions changed, not logging %s",meta$src)
          return()
        }
        cl <- cells(from = input, to = output)
        tm <- as.POSIXct(Sys.time())
        if ( .self$n == 0 ){
          .self$cells <- cl[,1,drop=FALSE]
          .self$t <- c(.self$t,tm)
          .self$expr <- ""
        }
        .self$n <- .self$n+1
        .self$t <- c(.self$t, tm)
        .self$expr <- c(.self$expr, meta$src)
        .self$cells <- cbind(.self$cells, cl[,2,drop=FALSE])
      }
    , dump = function(file=NULL,verbose=TRUE,...){
      "Dump logging info to csv file. 
       All arguments in '...' except row.names are passed to 'write.csv'"
      out <- .self$log_data()
      outf <- if( !is.null(file) ) file
              else if (.self$label == "" ) "lbj_cells.csv" 
              else paste0(.self$label, "_lbj_cells.csv")
      write.csv(out, file=outf, row.names=FALSE,...)
      .self$fmsg("Dumped a log at %s", normalizePath(outf))
    }
    ,  log_data = function(){
      "Return logged data as a data.frame"
      out <- data.frame(
        step = if(.self$n > 0 ) 0:.self$n else integer(0)
        , time = .self$t
        , expression = .self$expr
      )
      cl <- t(.self$cells)
      row.names(cl) <- NULL
      cbind(out, cl)
    }
    , show = function(){
      "Print method"
      cat("Logging object of class lbj_cells with the following logging info\n")
      print(.self$log_data())
    }
    , fmsg = function(fmt,...){
      if (.self$verbose){
        message(sprintf(fmt,...))
      }
    }
    )
)

#' Logging object to use with the lumberjack package
#'
#' @family loggers
#' @export lbj_rules
#' @exportClass lbj_rules
lbj_rules <- setRefClass("lbj_rules",
  fields = list(
    compare = "array"
    , rules = "validator"
    , n = "numeric"
    , t = "POSIXct"
    , expr = "character"
    , verbose = "logical"
    , label = "character"
  )
  , methods = list(
    initialize = function(rules, verbose=TRUE, label=""){
      "Create object. Optionally toggle verbosity."
      .self$n        <- 0
      .self$t        <- .POSIXct(numeric(0))
      .self$verbose  <- verbose
      .self$rules    <- rules$copy()
      .self$label    <- label
      v              <- validator(x>0)
    }
    , add = function(meta, input, output){
      if (!identical(dim(input),dim(output))){
        warnf("dimensions changed, not logging %s",meta$src)
        return()
      }
      tm <- as.POSIXct(Sys.time())
        comp <- cbind(compare(.self$rules, input, output))
        if ( .self$n == 0 ){
          .self$compare <- comp[,1,drop=FALSE]
          .self$t <- c(.self$t,tm)
          .self$expr <- ""
        }
        .self$n <- .self$n+1
        .self$t <- c(.self$t, tm)
        .self$expr <- c(.self$expr, meta$src)
        .self$compare <- cbind(.self$compare, comp[,2,drop=FALSE])
    }
    , dump = function(file=NULL,...){
      "Dump logging info to csv file. 
       All arguments in '...' except row.names are passed to 'write.csv'"
      out <- .self$log_data()
      outf <- if( !is.null(file) ) file
              else if (.self$label == "" ) "lbj_rules.csv" 
              else paste0(.self$label, "_lbj_rules.csv")
      write.csv(out, file=outf, row.names=FALSE,...)
      .self$fmsg("Dumped a log at %s", normalizePath(outf))
    }
    ,  log_data = function(){
      "Return logged data as a data.frame"
      out <- data.frame(
        step = if(.self$n > 0 ) 0:.self$n else integer(0)
        , time = .self$t
        , expression = .self$expr
      )
      cm <- t(.self$compare)
      row.names(cm) <- NULL
      cbind(out, cm)
    }
    , show = function(){
      "Print method"
      cat("Logging object of class lbj_rules with the following logging info\n")
      print(.self$log_data())
    }
    , plot = function(){
      "plot rule comparisons"
       pl <- getMethod("plot","validatorComparison")
       log <- .self$log_data()
       cmp <- t(log[-(1:3)])
       class(cmp) <- c("validatorComparison", "array")
       x <- gsub("\\(.*","",log$expression)
       colnames(cmp) <- x
       pl(cmp)
    }
    , fmsg = function(fmt,...){
      if (.self$verbose){
        message(sprintf(fmt,...))
      }
    }
    )
)

