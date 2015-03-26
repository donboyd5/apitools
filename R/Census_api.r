

#' @title Get a Dataset from the Census eits API
#'
#' @description
#' \code{Census_eits} returns a data frame with a list of available datasets
#'
#' @usage Census_eits(dsname, key)
#' @param dsname text in c("qtax", "qpr", "vip") - quarterly tax, quarterly public retirement systems, value in place
#' @param key Your Census API key (can be obtained for free - check www.census.gov)
#' @details
#' Queries the Census API to get the requested
#' @return data frame
#' @keywords Census_eits
#' @export
#' @examples
#' # don't need to give it a key if you have yours set
#' cen.qtax <- Census_eits("qtax")
#' cen.qpr <- Census_eits("qpr")
#' cen.vip <- Census_eits("vip")
Census_eits <- function(dsname, key=Census_defaultkey()) {
  urlpre <- paste0(Census_url(), dsname, "?&key=", key)
  urlpost <- "&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data"
  url <- paste0(urlpre, urlpost)
  result <- jsonlite::fromJSON(url)
  df <- data.frame(result)
  names(df) <- t(df[1, ])
  df2 <- df[-1, ] %>% mutate(value=cton(cell_value)) %>% select(-cell_value)
  if(dsname %in% c("qtax", "qpr")) df2$date <- getcenqdate(df2$time_slot_name) else
    if(dsname=="vip") df2$date <- getcenmdate(df2$time_slot_name)
  return(df2)
}


