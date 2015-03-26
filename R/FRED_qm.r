# Get selected data from FRED using quantmod, as it is faster than going to the BEA API
# Maybe don't do this on a day when new data are being released as I am not sure how fast
# FRED gets updated

# This is not really an API but it is closely related


#' @title Query FRED via quantmod to get a single variable
#'
#' @description
#' \code{FRED} returns a data frame with requested regional data. Right now only works with annual.
#' @usage FRED(var, freq)
#' @param var a variable name in c("gdp", "gdppi", "rgdp")
#' @param freq "q" or "a"; default: "q"
#' @details
#' Queries FRED via quantmod to get the data.
#' @return data frame with data
#' @keywords FRED
#' @export
#' @examples
#' FRED()
#' df <- FRED("gdppi", "q")
#' tail(df)
#' tail(NIPA("gdppi", "q"))
#' df <- FRED("gdp", "a")
#' tail(df)
#' tail(NIPA("gdp", "a"))
#' df <- FRED("rgdp")
#' tail(df)
#' tail(NIPA("rgdp"))
FRED <- function(var="help", freq="q") {
  # NOTE: freq should be q or a
  vars <- c("gdp", "gdppi", "rgdp")
  if(!var %in% vars) {
    print("Available variables are: ")
    print(vars)
    return()
  }

  if(var=="gdp" & freq=="q") sym <- "GDP" else
    if(var=="gdp" & freq=="a") sym <- "GDPA" else
      if(var=="gdppi" & freq=="q") sym <- "GDPCTPI" else
        if(var=="gdppi" & freq=="a") sym <- "B191RG3A086NBEA" else
          if(var=="rgdp" & freq=="q") sym <- "GDPC1" else
            if(var=="rgdp" & freq=="a") sym <- "GDPCA"

  df <- as.data.frame(quantmod::getSymbols(sym, src="FRED", auto.assign=FALSE)) %>%
    mutate(value=.[, 1], timeperiod=rownames(.), variable=var, symbol=sym)

  if(toupper(freq)=="Q") df <- select(df, date=timeperiod, variable,  symbol, value) else
    if(toupper(freq)=="A") df <- mutate(df, year=lubridate::year(timeperiod)) %>% select(year, variable, symbol, value)
    return(df)
}

