% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apitools.r
\docType{package}
\name{apitools}
\alias{apitools}
\alias{apitools-package}
\title{apitools}
\description{
Tools for using the APIs of the U.S. Bureau of Economic Analysis (BEA -- www.bea.gov) and
the Census Bureau (www.census.gov).

For information about the BEA API, see http://www.bea.gov/API/bea_web_service_api_user_guide.htm.

For details on BEA NIPA datasets see:\cr
\itemize{
  \item NIPA:  http://www.bea.gov/iTable/index_nipa.cfm
  \item NIPA underlying detail: http://www.bea.gov/iTable/index_UD.cfm
}
}
\section{Overview}{


Basic steps for BEA data:
\enumerate{
  \item BEA_DSlist() to get a list of available datasets
  \item Decide upon a dataset of interest
  \item BEA_DSparams() to get the parameters needed for the dataset of interest
  \item If you need information about allowable parameters, use BEA_ParamVals()
}
}

\section{Example -- NIPA data}{

bds <- BEA_DSlist() # get a list of available datasets\cr
bds

BEA_DSparams("NIPA")  # what parameters does the NIPA dataset require?\cr
# note that Year=X gets all years - that is what is built into apitools\cr
# TableID is the main parameter we need to know about; note that the API will NOT accept multiple TableIDs and it does not have AllValue

tablist <- BEA_ParamVals("NIPA", "TableID")  # get a list of the available TableIDs\cr
glimpse(tablist) # ok, we have to find the TableID of a BEA table we want\cr
# assume we went to http://www.bea.gov/iTable/index_nipa.cfm and saw that Table 1.1.4. has the price indexes we want; now find it\cr
filter(tablist, grepl("Table 1.1.4.", Description)) # TableID 4 has the info we want

nipadat <- NIPA_Data(4, "q") # get quarterly data from TableID 4, all years\cr
head(nipadat)\cr
count(nipadat, TableID, SeriesCode, LineNumber, LineDescription) # find all the variables

# we know from the above and from http://www.bea.gov/iTable/iTable.cfm?ReqID=9&step=1#reqid=9&step=3&isuri=1&903=4
that LineNumber 26 has the price index for State & local government consumption\cr
slgpi <- nipadat %>% filter(LineNumber==26)\cr
tail(slgpi)
}

\section{Example -- get multiple NIPA tables}{

# Start by getting a vector of TableIDs based on their BEA formal Table names

nipatabvec <- c("2.1", "2.3.4", "2.3.6", "3.3", "3.9.6", "3.10.6")\cr
df <- NIPA_DataMult(nipatabvec) # this can take a while\cr
head(df)\cr
count(df2, TableID, Description)
}

