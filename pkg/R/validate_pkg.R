#' Validation rules for data
#' 
#' Data often contain errors and missing data. A necessary step before data analysis
#' is verifying and validating your data. 
#' Package \code{validate} is a toolbox for creating validation rules and checking
#' data with these rules. 
#' 
#' Validation in \code{validate} follows the pattern of:
#' \itemize{
#'   \item Defining a set of \code{\link{validator}} and/or
#'    \code{\link{indicator}} rules
#'   \item Checking/\code{\link{confront}}ing data with the 
#'   validator and indicator rules
#'   \item Examining the results of a confrontation by looking at 
#'   \code{\link{validation}}s and \code{\link{indication}}s
#' }
#'    
#' @docType package
#' @name validate
#' @aliases package-validate validate
NULL