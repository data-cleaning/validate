#' Get code list from an SDMX REST API endpoint.
#' 
#' Constructs an URL for \code{rsdmx::readSDMX} and extracts the code IDs.
#' Code lists are downloaded once and cached for the duration of the R session.
#' 
#' @param endpoint \code{[character]} REST API endpoint of the SDMX registry
#' @param agency_id \code{[character]} Agency ID (e.g. \code{"ESTAT"})
#' @param resource_id \code{[character]} Resource ID (e.g. \code{"CL_ACTIVITY"})
#' @param version  \code{[character]} Version of the code list.
#' @param what \code{[character]} Return a \code{character} with code id's, or
#'        a data frame with all information.
#' @param ... passed to \code{\link[rsdmx]{readSDMX}}
#'
#' @family sdmx
#' @export
#' 
#' @examples
#' 
#' # We wrap the example in a 'try' statement to avoid failure on CRAN in case
#' # the service is off-line.
#' 
#'  # here we download the CL_ACTIVITY codelist from the  ESTAT registry.
#' try({
#'  codelist <- sdmx_codelist(
#'    endpoint = "https://registry.sdmx.org/ws/public/sdmxapi/rest/"
#'    , agency_id = "ESTAT"
#'    , resource_id = "CL_ACTIVITY"
#'  )
#' })
sdmx_codelist <- local({
  store <- new.env()
  
  function(endpoint, agency_id, resource_id, version="latest", what=c("id","all"), ...){
    if (!require('rsdmx', quietly=TRUE)){
      stop("You need to install 'rsdmx' to use SDMX functionality")
    }
    what <- match.arg(what)

    url <- file.path(endpoint, "codelist", agency_id, resource_id, version)
    if (!url %in% ls(store)){
      store[[url]] <- tryCatch(rsdmx::readSDMX(url, ...), error=function(e){
          msg <- sprintf(
            "retrieving data from \n'%s'\n failed with '%s'\n", url, e$message)
          stop(msg, call.=FALSE)
        })
    }
    
    df <- as.data.frame(store[[url]])
    if (what=="all") df else df[,1]
  }
})

#' Get code list from Eurostat SDMX repository
#' 
#' Convenience wrapper that calls \code{\link{sdmx_codelist}}.
#' 
#' @param resource_id \code{[character]} Resource ID (e.g. \code{"CL_ACTIVITY"})
#' @param version  \code{[character]} Version of the code list.
#' @param agency_id \code{[character]} Agency ID (default: \code{"ESTAT"}).
#' @param ... passed to \code{\link{sdmx_codelist}}
#'
#'
#' @family sdmx
#' @export
#'
#' @examples
#' # wrapped in a 'try' to avoid failure on CRAN in case the
#' # service is offline.
#' try( estat_codelist("CL_ACTIVITY") )
#'
estat_codelist <- function(resource_id, agency_id = "ESTAT", version="latest", ...){
  sdmx_codelist(
    endpoint      = endpoint("ESTAT")
    , agency_id   = "ESTAT"
    , resource_id = resource_id
    , version     = version
    , ...
  )   
}

#' Get code list from global SDMX repository
#' 
#' Convenience wrapper that calls \code{\link{sdmx_codelist}}.
#'
#' @inheritParams  estat_codelist
#'
#' @family sdmx
#' @export
#'
#' @examples
#' # wrapped in a 'try' to avoid failure on CRAN in case the
#' # service is offline.
#' try( global_codelist("CL_ACTIVITY", agency_id="ESTAT") )
global_codelist <- function(resource_id, agency_id, version="latest", ...){
  sdmx_codelist(
    endpoint      = endpoint("GLOBAL")
    , agency_id   = "GLOBAL"
    , resource_id = resource_id
    , version     = version
    , ...
  )   
}



# list of endpoints
# if 'registry' is missing, the list of supported endpoints is returned
endpoint <- function(registry=NULL){
  ENDPOINTS <- c(
      "ESTAT"  = "https://ec.europa.eu/tools/cspa_services_global/sdmxregistry/rest" 
    , "GLOBAL" = "https://registry.sdmx.org/ws/public/sdmxapi/rest"
  )

  if (is.null(registry)) return(ENDPOINTS)

  out <- ENDPOINTS[registry] 
  if (is.na(out)) stop(sprintf("Unrecognized registry, use one of %s"
                    , paste(names(ENDPOINTS), collapse=", ")))
  else out

}



