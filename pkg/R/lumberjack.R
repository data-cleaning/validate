
#' Logging object to use with the lumberjack package
#'
#' @section Details: 
#'
#' This obeject can used with the pipe operator of the 
#' \code{\link[lumberjack]{lumberjack}} package. The logging
#' is based on validate's \code{\link{cells}} function. The output
#' is written to a \code{csv} file wich contains the following columns.
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
#' @usage 
#' lumberjack::start_log(data, log=cell_counts(verbose=TRUE))
#' lumberjack::dump(data, file="cells.csv",...)
#' 
#' @param data A \code{data.frame}
#' @param log A logging object
#' @param verbose Toggle logger verbosity
#' @param file Filename to dump the log to
#' @param ... Parameters passed to \code{\link{write.csv}}
#'
#' @docType class
#' @format A reference class object
#'
#'
#' @export
cell_counts <- setRefClass("lumberjack.cells"
    , fields = list(
        cells = "array"
        , n     = "numeric"
        , t     = "POSIXct"
        , expr  = "character"
        , verbose = "logical"
        )
    , methods = list(
      initialize = function(verbose=FALSE){
        .self$n <- 0
        .self$t <- .POSIXct(numeric(0))
        .self$verbose = verbose
      }
      , add = function(meta, input, output){
        cl <- cells(input, output)
        tm <- as.POSIXct(Sys.time())
        if ( .self$n == 0 ){
          .self$cells <- cl[,1,drop=FALSE]
          .self$t <- c(.self$t,tm)
          .self$expr <- ""
        }
        .self$n <- .self$n+1
        .self$t <- c(.self$t, tm)
        .self$expr <- c(.self$expr, meta$src)
        .self$cells <- cbind(.self$cells, cl[,2])
      }
    , dump = function(file="cells.csv",...){
        out <- data.frame(
          step = seq(0,.self$n)
          , time = .self$t
          , expr = .self$expr
        )
        cl <- t(.self$cells)
        row.names(cl) <- NULL
        out <- cbind(out, cl)
        write.csv(out, file=file, row.names=FALSE,...)
        .self$fmsg("Dumped a log at %s", normalizePath(file))
    }
    , fmsg = function(fmt,...){
      if (.self$verbose){
        message(sprintf(fmt,...))
      }
    }
    )
)


