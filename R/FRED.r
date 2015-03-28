
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
    if(ddiff>=0 & ddiff<7) freq <- "D" else
      if(ddiff>=27 & ddiff<=32) freq <- "M" else
        if(ddiff>=85 & ddiff<=95) freq <- "Q" else
          if(ddiff>=360 & ddiff<=370) freq <- "A" else
            freq <- "Unknown"
    df <- df %>% mutate(freq=freq, year=lubridate::year(date)) # get the year for all data
    return(df)
  }

  df2 <- data.frame(series=svec) %>%
    group_by(series) %>%
    do(getoneseries(.$series))
  if(!full) df2 <- df2 %>% select(date, year, freq, value)

  return(df2)
}


#' @title Get info on variables in one or more FRED categories
#'
#' @description
#' \code{FREDcat} returns a data frame with info on series
#'
#' @usage FREDcat(catvec, key)
#' @param catvec numeric vector with list of FRED categories to get info for
#' @param key Your FRED API key (can be obtained for free - check http://api.stlouisfed.org/api_key.html)
#' default returns data frame with info on series available in one or several categories
#' @details
#' Queries the FRED API to get the requested data
#' @return data frame
#' @keywords FREDcat
#' @export
#' @examples
#' require(dplyr)
#' df <- FREDcat(c(106, 125))
#' df <- FREDcat(18) # 18 is NIPA, so this will get info on ALL series in the NIPA category and its children
#' df2 <- df %>% filter(grepl("Billions of Chained 2009 Dollars", units),
#'               grepl("consumption", title, ignore.case=TRUE),
#'               grepl("durable", title, ignore.case=TRUE)) %>%
#'        select(cat, id, title, frequency_short, seasonal_adjustment_short, units_short)
#' # Look at df2 in the viewer to find the desired consumption variables
#' # Let's look at PCDGCC96 real quarterly durables, and PCNDGC96 real q nondurables
#' head(df)
#' count(df, id, title)
#' vars <- c("PCDGCC96", "PCNDGC96")
#' FRED(vars)
FREDcat <- function(catvec, key=FRED_defaultkey()) {
  # get info on all series in all categories and children of those categories in catvec
  fredroot <- "http://api.stlouisfed.org/fred/"
  fredpost <- paste0("&api_key=", key, "&file_type=json")

  getonecategory <- function(cat) {
    # get info on all series in one category
    catinfo <- paste0("category/series?category_id=", cat)
    url <- paste0(fredroot, catinfo, fredpost)
    result <- jsonlite::fromJSON(url)
    df <- data.frame(result$seriess) %>% select(-contains("realtime"))
    return(df)
  }

  getallcatchildren <- function(catvec) {
    # get vector of all the children categories for each category in catvec
    getonecatschildren <- function(cat) {
      url <- paste0(fredroot, "category/children?category_id=", cat, fredpost)
      df2 <- data.frame(jsonlite::fromJSON(url))
      if(nrow(df2)==0) df2 <- data.frame(categories.id=cat)
      return(df2)
    }
    # get vector with all children of all categories in catvec
    df <- data.frame(cat=catvec) %>%
      group_by(cat) %>%
      do(getonecatschildren(.$cat))

    catout <- unique(df$categories.id)
    return(catout)
  }

  # Here is where the real work gets done
  catvec2 <- getallcatchildren(catvec) # get the full category vector
  # now get info on all series in the full category vector
  df2 <- data.frame(cat=catvec2) %>%
    group_by(cat) %>%
    do(getonecategory(.$cat))

  return(df2)
}




