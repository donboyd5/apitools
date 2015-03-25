

# See http://www.bea.gov/API/bea_web_service_api_user_guide.htm for table ids; also, use functions below

# BEA NIPA data
# NIPA:  http://www.bea.gov/iTable/index_nipa.cfm
# NIPA underlying detail: http://www.bea.gov/iTable/index_UD.cfm


# Define data to get:
#   TableID 1 has GDP (e.g., pch)
#   TableID 88 has govt
#   TableID 4 has price indexes
#
#   Year=X gets all years


#' @title Get a List of Datasets Available from the BEA API
#'
#' @description
#' \code{BEA_DSlist} returns a data frame with a list of available datasets
#'
#' @usage BEA_DSlist(your_bea_key)
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov)
#' @details
#' Queries the BEA API to get a list of available datasets
#' @return data frame with columns DatasetName and DatasetDescription
#' @keywords BEA_DSlist
#' @export
#' @examples
#' bea.ds <- BEA_DSlist() # don't need to give it a key if you have yours set
BEA_DSlist <- function(key=BEA_defaultkey()){
  require(RCurl)
  require(jsonlite)
  url <- paste0(BEA_url(), "?&UserID=", key, "&method=GETDATASETLIST&")
  result <- getURL(url, .opts=curlOptions(followlocation=TRUE)) # get character var with all the info, in JSON format
  # fromJSON(results) # to see full structure
  fromJSON(result)$BEAAPI$Results$Dataset # returns a data frame
}


#' @title Get Parameters for a Particular BEA Dataset
#'
#' @description
#' \code{BEA_DSparams} returns a data frame with a list of parameters for the particular dataset
#' Pay particular attention to whether a data set will accept multiple table flags, or whether tables must be
#' retrieved one by one
#'
#' @usage BEA_DSparams(dsname, your_bea_key)
#' @param dsname text name of the dataset (see \code{BEA_DSlist})
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov)
#' @details
#' Queries the BEA API to get list of parameters needed to retrieve data for a particular dataset. Particularly useful datasets are NIPA and NIUnderlyingDetail
#' @return data frame with columns:
#'   "ParameterName"
#'   "ParameterDataType"
#'   "ParameterDescription"
#'   "ParameterIsRequiredFlag"
#'   "MultipleAcceptedFlag"
#'   "AllValue"
#'   "ParameterDefaultValue"
#' @keywords BEA_DSparams
#' @export
#' @examples
#' bea.param <- BEA_DSparams("NIPA") # don't need to give it a key if you have yours set
#' BEA_DSparams("NIUnderlyingDetail")
BEA_DSparams <- function(dsname, key=BEA_defaultkey()) {
  require(RCurl)
  require(jsonlite)
  url <- paste0(BEA_url(), "?&UserID=", key, "&method=GetParameterList&datasetname=", dsname,"&")
  result <- getURL(url, .opts=curlOptions(followlocation=TRUE)) # get character var with all the info, in JSON format
  df <- fromJSON(result)$BEAAPI$Results$Parameter # returns a data frame
  return(df)
}



#
# #  Get parameter values for a parameter, for each data set -- IMPORTANT for the parameter TableID ####
#' @title Get Parameter Values for a Parameter in a Chosen BEA Dataset
#'
#' @description
#' \code{BEA_ParamVals} returns a data frame with a list of parameters for the particular dataset
#' Pay particular attention to whether a data set will accept multiple table flags, or whether tables must be
#' retrieved one by one
#'
#' @usage BEA_ParamVals(dsname, yourbeakeyhere)
#' @param dsname text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist})
#' @param pname text name of the parameter of interest (e.g., "TableID" for the dataset "NIPA")  (see \code{BEA_DSparams})
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov)
#' @details
#' Queries the BEA API to get allowable values for a parameter of interest for a dataset of interest. Particularly useful datasets are NIPA and NIUnderlyingDetail
#' @return data frame with information about allowable values for the parameter of interest
#' @keywords BEA_ParamVals
#' @export
#' @examples
#' nipa.tablist <- BEA_ParamVals("NIPA", "TableID") # don't need to give a key if you have yours set
#' head(nipa.tablist)
#' nipa.uld.tablist <- BEA_ParamVals("NIUnderlyingDetail", "TableID") # don't need to give a key if you have yours set
#' head(nipa.uld.tablist)
BEA_ParamVals <- function(dsname, pname, key=BEA_defaultkey()) {
  require(RCurl)
  require(jsonlite)
  url <- paste0(BEA_url(), "?&UserID=", key,
                "&method=GetParameterValues&datasetname=", dsname,
                "&ParameterName=", pname,"&")
  result <- getURL(url, .opts=curlOptions(followlocation=TRUE)) # get character var with all the info, in JSON format
  df <- fromJSON(result)$BEAAPI$Results$ParamValue # returns a data frame
  return(df)
}


