

# Functions to get the Federal Reserve Board's Flow of Funds Z1 data
# Typically, just run getz1all to download and parse the data, returning a data frame

downloadz1 <- function(z1dir=tempdir()) {
  # Download the Federal Reserve Board's latest Z1 Flow of Funds data
  # ONLY DOWNLOAD WHEN NEW DATA ARE AVAILABLE. MAKE SURE ANY DESIRED ARCHIVAL VERSIONS ARE SAVED ELSEWHERE
  # If a permanent copy of the Z1 archive is desired, provide a directory, or use system
  # commands to copy from tempdir() to a permanent location

  # Guide to the data:
  # http://www.federalreserve.gov/apps/fof/SeriesStructure.aspx
  # also, get the Z1 coded tables, to inspect how the numbers fit together

  print("Downloading the FRB Z1 file, could take a while...")
  z1url <- "http://www.federalreserve.gov/datadownload/Output.aspx?rel=Z1&filetype=zip"
  z1dir <- gsub("\\\\$", "", z1dir) # remove any trailing slashes from z1dir so that we can safely add a slash
  z1file <- paste0(z1dir, "/", "FRB_Z1.zip")
  download.file(z1url, z1file, mode="wb")
  return()
}


getz1vec <- function(z1dir=tempdir()) {
  # Read FRB Z1 xml data into a character matrix. We do this because the fastest way to work with these
  # data seems to be to parse the data directly, rather use one of the xml parsers in an R package, and
  # a character vector of the data is needed for that.
  # Assumes the data are in the tempdir, but can provide an alternative dir
  print("Reading large FRB Z1 file, could take a while...")
  z1dir <- gsub("\\\\$", "", z1dir) # remove any trailing slashes from z1dir so that we can safely add a slash
  con <- unz(paste0(z1dir, "/", "FRB_Z1.zip"), "Z1_data.xml") # read directly from zip file
  z1vec <- scan(con, what="", sep="\n", nlines=-1)
  close(con)
  return(z1vec)
}


getvardf <- function(z1vec) {
  # get a data frame with info on variables - one row per variable - from the z1 vector
  print("Getting data frame with information about each variable...")
  # get series names and frequencies
  isname <- which(stringr::str_sub(z1vec, 1, 10)=="<kf:Series")
  temp <- z1vec[isname] # put the name and frequency info into temp, and then parse it
  nstart <- regexpr("SERIES_NAME=", temp, fixed=TRUE) # get the start position for the series name - may not be identical
  sname <- stringr::str_sub(temp, nstart + 13, nstart + 25) # series name
  freq <- toupper(stringr::str_sub(sname, -1))

  # get series description information
  # first, get indexes for series description
  isdesc <- which(stringr::str_sub(z1vec, 1, 39)=="<common:AnnotationType>Long Description") + 1
  sdesc <- stringr::str_sub(z1vec[isdesc], 24, nchar(z1vec[isdesc]) - 2)
  sdesc <- stringr::str_sub(sdesc, 1, regexpr("<", sdesc, fixed=TRUE) - 1)

  # find the start and end of each data series
  sstart <- which(z1vec=="</frb:Annotations>") + 1 # slash-frbanno is right BEFORE the start of each series
  send <- which(z1vec=="</kf:Series>") - 1 # slash-kfseries is right AFTER end of each series
  nobs <- send - sstart + 1 # number of observations
  vars <- data_frame(variable=sname, freq, description=sdesc, sstart, send, nobs, rownum=1:length(sname))
  return(vars)
}


cleandate <- function(df) {
  getfdq <- function(date) {
    # get first day of quarter from a date that is first day of final month in quarter
    fmq <- function(m) return(trunc((m-1)/3) * 3) # first month of the quarter, for months 0-11
    fdq <- as.POSIXlt(date)
    fdq$mon <- fmq(fdq$mon) # go to start of quarter
    fdq$mday <- 1
    fdq <- as.POSIXct((fdq))
    return(fdq)
  }

  getfda <- function(date) {
    # get first day of year from a date that is first day of final month in quarter
    fda <- as.POSIXlt(date)
    fda$mon <- 0 # first month of year - a zero in POSIX
    fda$mday <- 1
    fda <- as.POSIXct((fda))
    return(fda)
  }

  # use conditional indexing because ifelse is weird for dates and factors
  # df$date2 <- df$date
  df$date[df$freq=="Q"] <- getfdq(df$date[df$freq=="Q"])
  df$date[df$freq=="A"] <- getfda(df$date[df$freq=="A"])
  return(df)
}


#' @title Get ALL Federal Reserve Board Z1 Flow of Funds data from previously downloaded zip file
#'
#' @description
#' \code{getz1df.fromfile} Downloads latest Z1 data from the FRB site (http://www.federalreserve.gov/datadownload)
#' and returns a data frame with all Z1 data
#'
#' @usage getz1df.fromfile(z1dir)
#' @param z1dir defines a directory where the Z1 zip file will be stored; default is tempdir()
#' @details
#' Downloads the data, reads it with scan, parses data, and cleans data
#' @return data frame
#' @keywords getz1df.fromfile
#' @export
#' @examples
#' z1 <- getz1df.fromfile()
#' head(z1)
#' # or from existing fof dir: z1 <- getz1df.fromfile(fof)
getz1df.fromfile <- function(z1dir=tempdir()) {
  z1vec <- getz1vec(z1dir)
  vars <- getvardf(z1vec)

  # we need multiple copies of each variable row - one for each observation of that variable, to
  # facilitate linking variable info with the data observations
  mrg <- data_frame(rownum=rep(vars$rownum, times=vars$nobs)) # data frame with 1 record per data observation
  data.ids <- left_join(mrg, select(vars, rownum, variable, freq, description)) # add the variable info to the id file

  # get and clean the actual data
  getdf <- function(z1vec) {
    print("Getting date and data value for each observation, could take a while...")
    data <- z1vec[grepl("<frb:Obs", z1vec)]
    obsstart <- regexpr("OBS_VALUE=", data, fixed=TRUE)
    freqstart <- regexpr("TIME_PERIOD=", data, fixed=TRUE)
    vals <- stringr::str_sub(data, obsstart + 11, freqstart - 3)
    vals <- ifelse(vals=="-9999", NA, vals) # safest to do this BEFORE conversion to numeric
    timeperiod <- stringr::str_sub(data, freqstart + 13, -5)
    datadf <- data_frame(date=lubridate::ymd(timeperiod), value=cton(vals))
    return(datadf)
  }

  data.values <- getdf(z1vec)

  print("Linking variable metadata to variable data, could take a while...")
  z1data <- cbind(select(data.ids, -rownum), data.values)

  # Final cleanup
  # uses conditional indexing because ifelse is weird for dates and factors
  z1data <- cleandate(z1data)

  # when testing, may want to verify uniqueness
  # anyDuplicated(z1data)
  return(z1data)
}



# testing ground
# fof <- "E:\\Data\\FOF\\"
# z1 <- getz1df.fromfile(fof)

# z1data <- z1
# z1data <- cleandate(z1data)
# count(z1data, freq)
# str(z1data)
# ht(z1data)

# see http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
# safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))


#' @title Get latest version of ALL Federal Reserve Board Z1 Flow of Funds data from web
#'
#' @description
#' \code{z1all} Downloads latest Z1 data from the FRB site (http://www.federalreserve.gov/datadownload)
#' and returns a data frame with all Z1 data
#'
#' @usage z1all(z1dir)
#' @param z1dir defines a directory where the Z1 zip file will be stored; default is tempdir()
#' @details
#' Downloads the data, reads it with scan, parses data, and cleans data
#' @return data frame
#' @keywords z1all
#' @export
#' @examples
#' z1 <- z1all()
#' head(z1)
z1all <- function(z1dir=tempdir()) {
  downloadz1(z1dir)
  z1data <- getz1df.fromfile()
  return(z1data)
}

