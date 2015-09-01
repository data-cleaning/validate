#' Data Validation Infrastructure
#' 
#' @section Introduction:
#' Data often contain errors and missing data. A necessary step before data
#' analysis is verifying and validating your data. Package \code{validate} is a
#' toolbox for creating validation rules and checking data against these rules.
#' 
#' 
#' @section Getting started:
#' 
#' The easiest way to get started is through the examples given in \code{\link{check_that}}. 
#' 
#' The general workflow in \code{validate} follows the following pattern.
#' \itemize{
#'   \item Define a set of rules using \code{\link{validator}}. 
#'   \item \code{\link{confront}} data with the validation rules
#'   \item Examine the results either graphically or by summary.
#' }
#'
#' There are several convenience functions that allow one to define rules from the
#' commandline, through a (freeform or yaml) file and to investigate and maintain the 
#' rules themselves. Please have a look at the introductory vignette
#'
#' \code{vignette("intro",package="validate")}
#'
#' for a more thorough introduction. After you're a bit aqcuinted with the package,
#' you will probably be interested in defining your rules separately in a text file.
#' The vignette 
#' 
#' \code{vignette("rule-files",package="validate")}
#' 
#' will get you started with that.
#'
#'    
#' @docType package
#' @name validate
#' @useDynLib validate
#' @aliases package-validate validate
#' @import methods
#' @importFrom graphics legend par text
#' @importFrom utils getFromNamespace
#' @import settings
#' @import yaml
NULL
