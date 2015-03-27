
#' @title Get one or more series from FRED
#'
#' @description
#' \code{FRED} returns a data frame with series data
#'
#' @usage FRED(svec, key, full)
#' @param svec character vector with list of series
#' @param key Your FRED API key (can be obtained for free - check http://api.stlouisfed.org/api_key.html)
#' @param full determines whether to return all data including realtime dates
#' default returns series name, date, and value
#' @details
#' Queries the FRED API to get the requested data
#' @return data frame
#' @keywords FRED
#' @export
#' @examples
#' require(dplyr)
#' df <- FRED("GDP")
#' head(df)
#' df <- FRED(c("GDP", "GNP"))
#' tail(df)
#' vlist <- c("GDP", "GNP", "GDPCTPI", "GNPCA")
#' df <- FRED(vlist)
#' tail(df)
#' count(df, series)
#' count(df, freq)
FRED <- function(svec, key=FRED_defaultkey(), full=FALSE) {
  fredroot <- "http://api.stlouisfed.org/fred/"
  fredpost <- paste0("&api_key=", key, "&file_type=json")

  getoneseries <- function(series) {
    seriesinfo <- paste0("series/observations?series_id=", series)
    url <- paste0(fredroot, seriesinfo, fredpost)
    result <- jsonlite::fromJSON(url)
    df <- data.frame(result$observations) %>% mutate(value=cton(value),
                                                     date=as.Date(date))
                                                     #date=lubridate::ymd(date))
    firstnonna <- min(which(!is.na(df$value)))
    df <- df %>% filter(row_number() >= firstnonna)
    ddiff <- df$date[nrow(df)] - df$date[nrow(df)-1] # get number of days between last and 2nd to last observation
    # guess at the frequency of the data; probably can get this from metadata
    if(ddiff>=27 & ddiff<=32) freq <- "M" else
      if(ddiff>=85 & ddiff<=95) freq <- "Q" else
        if(ddiff>=360 & ddiff<=370) freq <- "A"
    df <- df %>% mutate(freq=freq, year=lubridate::year(date)) # get the year for all data
    return(df)
  }

  df2 <- data.frame(series=svec) %>%
    group_by(series) %>%
    do(getoneseries(.$series))
  if(!full) df2 <- df2 %>% select(date, year, freq, value)

  return(df2)
}

