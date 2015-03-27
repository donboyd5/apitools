


# http://api.stlouisfed.org/fred/series?series_id=GNPCA&api_key=abcdefghijklmnopqrstuvwxyz123456&file_type=json
# http://api.stlouisfed.org/fred/series?series_id=GNPCA&api_key=5fddcf6a32e3daca699415c9e529555f&file_type=json
# 5fddcf6a32e3daca699415c9e529555f

#' @title Get a series from FRED
#'
#' @description
#' \code{FRED2} returns a data frame with series data
#'
#' @usage FRED2(series, key)
#' @param series text name of series
#' @param key Your FRED API key (can be obtained for free - check http://api.stlouisfed.org/api_key.html)
#' @details
#' Queries the FRED API to get the requested data
#' @return data frame
#' @keywords FRED2
#' @export
FRED2 <- function(series, key=FRED_defaultkey()) {
  fredroot <- "http://api.stlouisfed.org/fred/"
  seriesinfo <- paste0("series?series_id=", series)
  urlpost <- paste0("&api_key=", key, "&file_type=json")
  url <- paste0(fredroot, seriesinfo, urlpost)
  result <- jsonlite::fromJSON(url)
  df <- data.frame(result)
  return(df)
}
