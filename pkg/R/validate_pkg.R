#' Data Validation Infrastructure
#' 
#' @section Introduction:
#' Data often contain errors and missing data. A necessary step before data
#' analysis is verifying and validating your data. Package \code{validate} is a
#' toolbox for creating validation rules and checking data against these rules.
#' 
#' Secondly, the package offers ways to create and maintain so-called indicators,
#' that are particularly useful to follow the condition of a data set as it gets treated
#' during cleaning and transformation. 
#' 
#' @section Getting started:
#' 
#' The easiest way to get started is through the examples given in \code{\link{check_that}}. 
#' 
#' The general workflow in \code{validate} follows the pattern of: 
#' \itemize{
#'   \item Defining a set of \code{\link{validator}} and/
#'    \code{\link{indicator}} rules
#'   \item \code{\link{confront}}ing data with the validator and indicator rules
#'   \item Examining the results either graphically or by summary.
#' }
#'
#' There are several convenience functions that allow one to define rules from the
#' commandline, through a (freeform or yaml) file and to investigate and maintain the 
#' rules themselves.
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
