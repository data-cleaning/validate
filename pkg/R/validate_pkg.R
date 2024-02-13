#' Data Validation Infrastructure
#'
#' 
#' Data often suffer from errors and missing values. A necessary step before data
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
#' \href{../doc/cookbook.html}{cookbook} for a comprehensive introduction.
#'
#' @references
#' An overview of this package, its underlying ideas and many examples
#' can be found in MPJ van der Loo and E. de Jonge (2018) \emph{Statistical
#' data cleaning with applications in R} John Wiley & Sons.
#' 
#' Please use \code{citation("validate")} to get a citation for (scientific)
#' publications.
#'
#' @aliases validate-package 
#' @name validate
#' @useDynLib validate, .registration=TRUE
#' @aliases package-validate validate
#' @import methods
#' @importFrom graphics legend par text axis abline lines strwidth
#' @importFrom utils getFromNamespace
#' @import settings
#' @import yaml
#' @import grid
"_PACKAGE"


