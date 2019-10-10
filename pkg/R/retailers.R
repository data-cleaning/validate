#' @name retailers
#' @aliases SBS2000
#' @title data on Dutch supermarkets
#' @description 
#'   Anonymized and distorted data on revenue and cost structure for
#'   60 retailers. Currency is in thousands of Euros. There are two
#'   data sets. The \code{SBS2000} dataset is equal to the \code{retailers}
#'   data set except that it has a record identifier (called \code{id}) column.
#'
#' \itemize{
#'  \item id: A unique identifier (only in SBS2000)
#'  \item size: Size class (0=undetermined)
#'  \item incl.prob: Probability of inclusion in the sample
#'  \item staff: Number of staff
#'  \item turnover: Amount of turnover
#'  \item other.rev: Amount of other revenue
#'  \item total.rev: Total revenue
#'  \item staff.costs: Costs assiciated to staff
#'  \item total.costs: Total costs made
#'  \item profit: Amount of profit
#'  \item vat: Turnover reported for Value Added Tax
#'}
#'
#' @docType data
#' @format A csv file, one retailer per row.
NULL


#' @name SBS2000
#' @rdname retailers
NULL

