

# Helper functions: don't export these -------------------------
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar)) # this is all we need from btools so define it

# date functions for BEA data ####
getyear <- function(TimePeriod) year <- as.numeric(substr(TimePeriod, 1, 4))

getdate <- function(TimePeriod) { # if quarterly data we want the first day of quarter
  TimePeriod <- as.character(TimePeriod)
  year <- as.numeric(substr(TimePeriod, 1, 4))
  qtr <- as.numeric(substr(TimePeriod, 6, 6))
  month <- qtr * 3 - 2
  date <- as.Date(ISOdate(year, month, 1))
  return(date)
}

# date functions for Census data ####

getcenqdate <- function(time_slot_name) {
  # return first day of quarter for timeslot of form Q42011
  time_slot_name <- as.character(time_slot_name)
  year <- as.numeric(stringr::str_sub(time_slot_name, 3, 6))
  qtr <- as.numeric(stringr::str_sub(time_slot_name, 2, 2))
  month <- qtr * 3 - 2
  date <- as.Date(ISOdate(year, month, 1))
  return(date)
}

getcenmdate <- function(time_slot_name) {
  # return first day of month for timeslot of form Sep2014
  mn <- match(stringr::str_sub(time_slot_name, 1, 3), month.abb)
  yr <- as.numeric(stringr::str_sub(time_slot_name, 4, 7))
  date <- as.Date(ISOdate(yr, mn, 1))
  return(date)
}

