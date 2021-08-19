
# do we have rsdmx installed?
check_rsdmx <- function(){
  if (!requireNamespace('rsdmx', quietly=TRUE)){
    stop("You need to install 'rsdmx' to use SDMX functionality", call.=FALSE)
  }
}



# download something from an SDMX registry and cash it.
download_sdmx <- local({
  store <- new.env()
  
  function(endpoint, resource, agency_id, resource_id, version="latest", ...){
    check_rsdmx()

    url <- file.path(endpoint, resource, agency_id, resource_id, version)
    if (!url %in% ls(store)){
      store[[url]] <- tryCatch(rsdmx::readSDMX(url, ...), error=function(e){
          msg <- sprintf(
            "retrieving data from \n'%s'\n failed with '%s'\n", url, e$message)
          stop(msg, call.=FALSE)
        })
    }
    out <- store[[url]]
    attr(out, "url") <- url
    out
  }
})



#' Get code list from an SDMX REST API endpoint.
#' 
#' \code{sdmx_codelist} constructs an URL for \code{rsdmx::readSDMX} and
#' extracts the code IDs.  Code lists are downloaded once and cached for the
#' duration of the R session.
#' 
#' @inheritParams validator_from_dsd
#' @param what \code{[character]} Return a \code{character} with code id's, or
#'        a data frame with all information.
#'
#' @family sdmx
#' @export
#' 
#' @examples
#'  
#' 
#'  # here we download the CL_ACTIVITY codelist from the  ESTAT registry.
#' \dontrun{
#'  codelist <- sdmx_codelist(
#'    endpoint = "https://registry.sdmx.org/ws/public/sdmxapi/rest/"
#'    , agency_id = "ESTAT"
#'    , resource_id = "CL_ACTIVITY" 
#' }
#'
sdmx_codelist <- function(endpoint, agency_id, resource_id, version="latest", what=c("id","all")){
  check_rsdmx()
  what <- match.arg(what)

  dl <- download_sdmx(endpoint, "codelist", agency_id, resource_id, version)
  
  df <- as.data.frame(dl)
  if (what=="all") df else df[,1]
}




#' Get code list from Eurostat SDMX repository
#' 
#' \code{estat_codelist} gets a code list from the REST API provided at
#' \code{ec.europa.eu/tools/cspa_services_global/sdmxregistry}. It is a
#' convenience wrapper that calls \code{sdmx_codelist}.
#'  
#' @rdname sdmx_codelist
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   estat_codelist("CL_ACTIVITY")
#' }
estat_codelist <- function(resource_id, agency_id = "ESTAT", version="latest"){
  sdmx_codelist(
    endpoint      = sdmx_endpoint("ESTAT")
    , agency_id   = agency_id
    , resource_id = resource_id
    , version     = version
  )   
}

#' Get code list from global SDMX repository
#' 
#' \code{global_codelist} gets a code list from the REST API provided at
#' \code{https://registry.sdmx.org/webservice/data.html}.  It is a convenience
#' wrapper that calls \code{sdmx_codelist}.
#'
#' @rdname sdmx_codelist
#' @export
#' @family sdmx
#' @examples
#' \dontrun{
#'   global_codelist("CL_AGE") )
#'   global_codelist("CL_CONF_STATUS")
#'   global_codelist("CL_SEX")
#' }
#' # An example of using SDMX information, downloaded from the SDMX global
#' # registry
#' \dontrun{
#'  # economic data from the country of Samplonia
#'  data(samplonomy)
#'  head(samplonomy)
#' 
#'  rules <- validator(
#'    , freq %in% global_codelist("CL_FREQ")
#'    , value >= 0
#'  )
#'  cf <- confront(samplonomy, rules) 
#'  summary(cf)
#'
#' }
global_codelist <- function(resource_id, agency_id = "SDMX", version="latest"){
  sdmx_codelist(
    endpoint      = sdmx_endpoint("GLOBAL")
    , agency_id   = agency_id
    , resource_id = resource_id
    , version     = version
  )   
}



#' Get URL for known SDMX registry endpoints
#'
#' Convenience function storing URLs for SDMX endpoints. 
#'
#' @param registry \code{[character]} name of the endpoint (case insensitve). If \code{registry}
#' is \code{NULL} (the default), the list of supported endpoints is returned.
#'
#' @family sdmx
#'
#' @examples
#' sdmx_endpoint()
#' sdmx_endpoint("ESTAT")
#' sdmx_endpoint("global")
#'
#' @export
sdmx_endpoint <- function(registry=NULL){
  ENDPOINTS <- c(
      "ESTAT"  = "https://ec.europa.eu/tools/cspa_services_global/sdmxregistry/rest" 
    , "GLOBAL" = "https://registry.sdmx.org/ws/public/sdmxapi/rest"
  )

  if (is.null(registry)) return(ENDPOINTS)
  registry <- toupper(registry)

  out <- ENDPOINTS[registry] 
  if (is.na(out)) stop(sprintf("Unrecognized registry, use one of %s"
                    , paste(names(ENDPOINTS), collapse=", ")))
  else out

}



#' Extract a rule set from an SDMX DSD file
#'
#' Data Structure Definitions contain references to code lists.
#' This function extracts those references and generates rules
#' that check data against code lists in an SDMX registry.
#'
#' @param endpoint \code{[character]} REST API endpoint of the SDMX registry
#' @param agency_id \code{[character]} Agency ID (e.g. \code{"ESTAT"})
#' @param resource_id \code{[character]} Resource ID (e.g. \code{"CL_ACTIVITY"})
#' @param version  \code{[character]} Version of the code list.
#'
#' @return An object of class \code{\link{validator}}.
#' @family sdmx
#' @export
validator_from_dsd <- function(endpoint, agency_id, resource_id, version="latest"){
  dsd <- download_sdmx(endpoint, "datastructure", agency_id, resource_id, version)
  dimensions <- slot(slot(dsd, "datastructures")[[1]], "Components")
  df <- as.data.frame(dimensions)

  cl_vars <- !is.na(df$codelist)

  template <- '%s %%in%% sdmx_codelist(endpoint  = "%s", agency_id = "%s", resource_id = "%s", version = "%s")'

  df1 <- df[cl_vars,]

  rules <- data.frame(
    rule = sprintf(template
      , df1$conceptRef
      , endpoint
      , df1$codelistAgency
      , df1$codelist
      , version=df1$codelistVersion)
    , name = paste0("CL_", df1$conceptRef)
    , origin = attr(dsd,"url")
    , description = sprintf("Code list from %s::%s %s", resource_id, df1$conceptRef, version)
  )

  validator(.data=rules)
}






