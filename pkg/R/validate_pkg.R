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
#'   \item Define a set of rules or quality indicator using \code{\link{validator}} or \code{\link{indicator}}. 
#'   \item \code{\link{confront}} data with the rules or indicators,
#'   \item Examine the results either graphically or by summary.
#' }
#'
#' There are several convenience functions that allow one to define rules from
#' the commandline, through a (freeform or yaml) file and to investigate and
#' maintain the rules themselves. Please have a look at the
#' \href{../doc/introduction.html}{introductory vignette} for a more
#' thorough introduction on validation rules and the
#' \href{../doc/indicators.html}{indicators vignette} for an introducion on
#' quality indicators. After you're a bit aqcuinted with the package, you
#' will probably be interested in defining your rules separately in a text file.
#' The vignette on \href{../doc/rule_files}{rule files} will get you
#' started with that.
#'
#'    
#' @docType package
#' @name validate
#' @useDynLib validate, .registration=TRUE
#' @aliases package-validate validate
#' @import methods
#' @importFrom graphics legend par text
#' @importFrom utils getFromNamespace
#' @import settings
#' @import yaml
NULL
