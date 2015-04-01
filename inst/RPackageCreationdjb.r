# RPackageCreation.r
# Don Boyd
# 3/25/2015

# DON'T RUN THIS FILE. STUDY IT, AND RUN SELECTED LINES.

# comment out:
# # source("E:\\Dropbox (Personal)\\RPrograms PC\\BoydStartup.r", echo=TRUE)
# in C:\Program Files\R\R-3.1.3\etc\Rprofile.site
# while building the package


# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#   maybe?? #' @importFrom utils head tail - in the ...-package.r file

# http://www.molecularecologist.com/2013/11/using-github-with-r-and-rstudio/
# https://www.rstudio.com/ide/docs/version_control/overview
# http://kbroman.github.io/github_tutorial/


# http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# http://adv-r.had.co.nz/Package-basics.html
# https://github.com/klutometis/roxygen#roxygen2

# http://cran.r-project.org/doc/manuals/R-exts.html#Data-in-packages
# https://sites.google.com/site/hackoutwiki/developers-corner/developing-r-packages

# The following SSH key was added to your account:
#
#   GitHub for Windows - Don-PC
# 75:4b:6d:82:41:88:79:28:e1:b2:7a:01:66:93:61:8f
#
# If you believe this key was added in error, you can remove the key and disable
# access at the following location:
#
#   https://github.com/settings/ssh

# http://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# http://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html

getOption("defaultPackages")

devtools::install_github("donboyd5/apitools")
library(apitools)

# Once the package is in full gear, here are the basic steps, DETAILED below
# library(devtools)
# library(roxygen2)
document()
install("apitools")
# then commit and push

# RUN ONCE: define packages to be listed under Imports (the use_package default) in DESCRIPTION
devtools::use_package("dplyr")
devtools::use_package("httr") # not needed I think?? try rerunning without this, and removing from DESCRIPTION
devtools::use_package("jsonlite")
devtools::use_package("lubridate")
devtools::use_package("quantmod")
devtools::use_package("RCurl")
devtools::use_package("readr")
devtools::use_package("stringr")


# http://tex.stackexchange.com/questions/125274/error-font-ts1-zi4r-at-540-not-found
# per post: I ran into this issue, and after some time spent banging my head against a post, I did reach a solution. After installing the package, run the following steps from the inconsolata README:
#   initexmf --update-fndb
#   initexmf --edit-config-file updmap
#
# The latter command should open updmap.cfg in your default editor, commonly Notepad.
# Add the line
#
# Map zi4.map
#
# to updmap.cfg, save and close. Then, in the command window, type
#
# initexmf --mkmaps

#  simply fyi, the following code is of potential interest
#' Chain together multiple operations
#' @importFrom dplyr %>% # or maybe magrittr
#' @name %>%
#' @export
#' @rdname chain
#' @usage lhs \%>\% rhs
NULL

# Before checking, go to: C:\Program Files\R\R-3.1.3\etc\Rprofile.site
# and comment out: source("E:\\Dropbox (Personal)\\RPrograms PC\\BoydStartup.r", echo=TRUE)

# Steps for creating, revising, documenting, installing, and uploading a package to github:
# 0.a Load needed packages
#       library(devtools)
#       library(roxygen2)  # make sure latest from github is installed:  devtools::install_github("yihui/roxygen2")
# 0.b Create the package directory if this is a new package:
#       create("apitools")
# 0.c Create a package overview file in the package's R subdirectory (e.g., apitools-package.r)
#     so that documentation is easily found from within R by, for example, typing ?apitools
# 1. Revise/add functions and documentation, etc. - see examples below
# 2. Create markup documentation for the revised package via:
#       document()
# 3. Install the package locally
#       setwd("..") # maybe not needed
#       install("apitools")
# 4. Commit changes locally to git
#       - select commit from the git button on toolbar above
#       - press commit to commit the changes to the local repository (git on my computer)
# 5. Upload revised package to github (see http://r-pkgs.had.co.nz/git.html#git)
#       - create a new repo on github if not already done:
#           go to: https://github.com/new
#           create a repo with the same name as the package (e.g., apitools)
#       - initialize the repo
#           open the git shell from within RStudio
#           push the repo from the PC to the web with these two commands:
#             git remote add origin https://github.com/donboyd5/apitools.git
#             git push -u origin master
#       - after this initialization, do pushes from within RStudio
#       - uid id donboyd5, pw is the 8 chars pw
# 5. Optionally install from github
#       devtools::install_github("donboyd5/apitools")
# 6. Due to apparent RStudio bug, best to exit RStudio and then re-enter to see the documentation for the revised package


# # remove.packages("apitools")
#
#
# # here is an example of how to document a function
#' @title Get Data from a NIPA Table or a NIPA Underlying Detail Table (NIUnderlyingDetail)
#'
#' @description
#' \code{NIPA_Data} returns a data frame with a list of parameters for the particular dataset
#' Pay particular attention to whether a data set will accept multiple table flags, or whether tables must be
#' retrieved one by one
#'
#' @usage NIPA_Data(tableid, freq, dsname, key, verbose)
#' @param tableid the BEA table identifier (see \code{BEA_ParamVals})
#' @param freq "q" or "a"
#' @param dsname text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist})
#' @param key Your BEA API key (can be obtained for free - check www.bea.gov)
#' @param verbose default is FALSE
#' @details
#' Queries the BEA API to get data
#' @return data frame with data
#' @keywords NIPA_Data
#' @export
#' @examples
#' # quarterly gdp percent change NIPA TableID 1 ####
#' head(NIPA_Data(1, "q"))
#' require(dplyr)
#' gdppch <- NIPA_Data(1, "q") %>% # NIPA is default dsname so don't have to specify it
#'   filter(SeriesCode=="A191RL") %>%
#'   select(date, gdppch=value)
#' head(gdppch)
NIPA_Data <- function(tableid, freq="q", dsname="NIPA", key=BEA_defaultkey(), verbose=FALSE) {
  # NOTE: freq should be q or a
  if(verbose) print(paste0("Getting TableID: ", tableid))

  upart1 <- paste0(BEA_url(), "?&UserID=", key)
  upart2 <- paste0("&method=GetData&datasetname=", dsname)
  upart3 <- paste0("&TableID=", tableid)
  upart4 <- paste0("&Frequency=", toupper(freq))
  upart5 <- paste0("&Year=X&ResultFormat=JSON&") # Year=X gets all years
  url <- paste0(upart1, upart2, upart3, upart4, upart5)
  result <- RCurl::getURL(url, .opts=RCurl::curlOptions(followlocation=TRUE)) # sometimes this slow

  df <- jsonlite::fromJSON(result)$BEAAPI$Results$Data %>%
    mutate(value=cton(DataValue), LineNumber=as.numeric(LineNumber))
  if(toupper(freq)=="A") df$year <- getyear(df$TimePeriod) else
    if(toupper(freq)=="Q") df$date <- getdate(df$TimePeriod)

    return(df)
}




# loads, if needed ####
library(btools) # library that I created - note that lubridate will mask a few of the functions in here, with better or different versions - mdy, month, year
library(apitools) # library I created
library(foreign) # various import and export routines - e.g., for reading Stata files
library(gdata) # for reading spreadsheets
library(plyr) # always load BEFORE loading dplyr; try to stop needing this
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
