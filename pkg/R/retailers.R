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
#' @family datasets
#'
#' @docType data
#' @format A csv file, one retailer per row.
NULL


#' @name SBS2000
#' @rdname retailers
NULL

#' @name samplonomy
#' @title Economic data on Samplonia
#' @description
#'    Simulated economic time series representing GDP, Import, Export and
#'    Balance of Trade (BOT) of Samplonia. Samplonia is a fictional Island
#'    invented by Jelke Bethelehem (2009). The country has 10 000 inhabitants.
#'    It consists of two provinces: Agria and Induston. Agria is a rural
#'    province consisting of the mostly fruit and vegetable producing district
#'    of Wheaton and the mostly cattle producing Greenham. Induston has four
#'    districts. Two districts with heavy industry named Smokeley and Mudwater.
#'    Newbay is a young, developing district while Crowdon is where the rich
#'    Samplonians retire. The current data set contains several time series
#'    from Samplonia's national accounts system in long format.
#'
#'    There are annual and quarterly time series on GDP, Import, Export and
#'    Balance of Trade, for Samplonia as a whole, for each province and each
#'    district. BOT is defined as Export-Import for each region and period;
#'    quarterly figures are expected to add up to annual figures for each
#'    region and measure, and subregions are expected to add up to their
#'    super-regions.
#'    
#'    \itemize{
#'      \item region:  Region (Samplonia, one if its 2 provinces, or one of its 6 districts)
#'      \item freq:    Frequency of the time series
#'      \item period:  Period (year or quarter)
#'      \item measure: The economic variable (gdp, import, export, balance)
#'      \item value:   The value 
#'    }
#'
#'    The data set has been endowed with the following errors.
#'    \itemize{
#'      \item For Agria, the 2015 GDP record is not present.
#'      \item For Induston, the 2018Q3 export value is missing (\code{NA})
#'      \item For Induston, there are two different values for the 2018Q2 Export
#'      \item For Crowdon, the 2015Q1 balance value is missing (\code{NA}).
#'      \item For Wheaton, the 2019Q2 import is missing (\code{NA}).
#'    }
#'
#' @family datasets
#'
#' @references
#' J. Bethlehem (2009), Applied Survey Methods: A Statistical Perspective. John
#' Wiley & Sons, Hoboken, NJ.
#'
#' @docType data
#' @format An RData file.
NULL

#' @name nace_rev2
#' @title NACE classification code table
#' @description 
#' Statistical Classification of Economic Activities. 
#'
#'
#' 
#'\itemize{ 
#'  \item Order \code{[integer]}
#'  \item Level \code{[integer]} NACE level
#'  \item Code  \code{[character]} NACE code
#'  \item Parent \code{[character]} parent code of \code{"Code"}
#'  \item Description \code{[character]}
#'  \item This_item_includes \code{[character]}
#'  \item This_item_also_includes \code{[character]}
#'  \item Rulings \code{[character]}
#'  \item This_item_excludes \code{[character]}
#'  \item Reference_to_ISIC_Rev._4 \code{[character]}
#'}
#'
#'
#' @family datasets
#' @seealso \code{\link{hierarchy}}
#' @docType data
#'
#'
#' @format A csv file, one NACE code per row.
NULL





