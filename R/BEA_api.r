

# See http://www.bea.gov/API/bea_web_service_api_user_guide.htm for table ids; also, use functions below

# BEA NIPA data
# NIPA:  http://www.bea.gov/iTable/index_nipa.cfm
# NIPA underlying detail: http://www.bea.gov/iTable/index_UD.cfm



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



#' @title Get Parameter Values for a Parameter in a Chosen BEA Dataset
#'
#' @description
#' \code{BEA_ParamVals} returns a data frame with a list of parameters for the particular dataset
#' Pay particular attention to whether a data set will accept multiple table flags, or whether tables must be
#' retrieved one by one
#'
#' @usage BEA_ParamVals(dsname, your_bea_key)
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
#' BEA_ParamVals("RegionalData", "KeyCode") # TPI_SI is Total personal income (state annual income)
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



#' @title Get Data from a NIPA Table or a NIPA Underlying Detail Table (NIUnderlyingDetail)
#'
#' @description
#' \code{NIPA_Data} returns a data frame with a list of parameters for the particular dataset
#' Pay particular attention to whether a data set will accept multiple table flags, or whether tables must be
#' retrieved one by one
#'
#' @usage NIPA_Data(tableid, freq, dsname, key)
#' @param tableid the BEA table identifier (see \code{BEA_ParamVals})
#' @param freq "q" or "a"
#' @param dsname text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist})
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov)
#' @details
#' Queries the BEA API to get data
#' @return data frame with data
#' @keywords NIPA_Data
#' @export
#' @examples
#' # quarterly gdp percent change NIPA TableID 1 ####
#' head(NIPA_Data(1, "q"))
#' gdppch <- NIPA_Data(1, "q") %>% # NIPA is default dsname so don't have to specify it
#'   filter(SeriesCode=="A191RL") %>%
#'   select(date, gdppch=value)
#' head(gdppch)
NIPA_Data <- function(tableid, freq="q", dsname="NIPA", key=BEA_defaultkey(), verbose=FALSE) {
  # NOTE: freq should be q or a
  require(RCurl)
  require(jsonlite)
  require(dplyr)
  if(verbose) print(paste0("Getting TableID: ", tableid))

  getyear <- function(TimePeriod) year <- as.numeric(substr(TimePeriod, 1, 4))

  getdate <- function(TimePeriod) { # if quarterly data we want the first day of quarter
    TimePeriod <- as.character(TimePeriod)
    year <- as.numeric(substr(TimePeriod, 1, 4))
    qtr <- as.numeric(substr(TimePeriod, 6, 6))
    month <- qtr * 3 - 2
    date <- as.Date(ISOdate(year, month, 1))
    return(date)
  }

  upart1 <- paste0(BEA_url(), "?&UserID=", key)
  upart2 <- paste0("&method=GetData&datasetname=", dsname)
  upart3 <- paste0("&TableID=", tableid)
  upart4 <- paste0("&Frequency=", toupper(freq))
  upart5 <- paste0("&Year=X&ResultFormat=JSON&") # Year=X gets all years
  url <- paste0(upart1, upart2, upart3, upart4, upart5)
  result <- getURL(url, .opts=curlOptions(followlocation=TRUE)) # sometimes this slow

  df <- fromJSON(result)$BEAAPI$Results$Data %>%
    mutate(value=cton(DataValue), LineNumber=as.numeric(LineNumber))
  if(toupper(freq)=="A") df$year <- getyear(df$TimePeriod) else
    if(toupper(freq)=="Q") df$date <- getdate(df$TimePeriod)

  return(df)
}



#' @title Get a Vector of TableIDs Corresponding to NIPA Table Numbers
#' @description
#' \code{NIPATableIDs} returns a vector of TableIDs corresponding to NIPA table numbers
#' @usage NIPATableIDs(nipatablist, dsname, key)
#' @param dsname text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist})
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov)
#' @details
#' Queries the BEA API to get a list of tables for a particular dsname, and then finds the TableIDs for the given NIPA tables
#' @return vector with TableIDs
#' @keywords NIPATableIDs
#' @export
#' @examples
#' nipatabvec <- c("2.1", "2.3.4", "2.3.6", "3.3", "3.9.6", "3.10.6")
#' NIPATableIDs(nipatabvec)
NIPATableIDs <- function(nipatabvec, dsname="NIPA", key=BEA_defaultkey()) {
  tablist <- BEA_ParamVals(dsname, "TableID") # get the list of tables for this dsname
  nipatabvec2 <- paste0("Table ", nipatabvec, ". ") # put NIPA names in form that ensures we are getting a unique table
  findtab <- function(nipaname) grep(nipaname, tablist$Description, ignore.case=TRUE) # find the TableID for a single NIPA table name
  idx <- sapply(nipatabvec2, findtab) # Get all the tableIDs: row indexes of tablist for our desired tables
  tabs <- tablist[idx, ] # these are the tables we want
  return(tabs)
}



#' @title Get a Data Frame With Multiple BEA Tables
#'
#' @description
#' \code{NIPA_DataMult} returns a data frame with multiple BEA tables
#' @usage NIPA_DataMult(nipatablenames, dsname, key)
#' @param nipatablenames a character vector with NIPA table numbers (see example below); default: no default
#' @param freq "q" or "a"; default: "q"
#' @param dsname text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist}); default: "NIPA"
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov); default: my key
#' @details
#' Queries the BEA API to get the data
#' @return vector with TableIDs
#' @keywords NIPA_DataMult
#' @export
#' @examples
#' nipatabvec <- c("2.1", "2.3.4", "2.3.6", "3.3", "3.9.6", "3.10.6")
#' df <- NIPA_DataMult(nipatabvec) # this can take a while
#' head(df)
#' count(df2, TableID, Description)
NIPA_DataMult <- function(nipatablenames, freq="q", dsname="NIPA", key=BEA_defaultkey()) {
  tlist <- NIPATableIDs(nipatablenames, dsname, key=BEA_defaultkey()) # get TableIDs for the nipatablenames
  df <- tlist %>% group_by(TableID, Description) %>%
    do(NIPA_Data(.$TableID, freq=freq, dsname=dsname, verbose=TRUE))
  return(df)
}



