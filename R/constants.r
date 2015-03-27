# Constants used by apitools
# I put them inside of functions so as not to get overwritten by the user but there probably is a better way
# I don't want these to be visible to the user
# 3/25/2015

BEA_url <- function() return("http://www.bea.gov/api/data")

BEA_defaultkey <- function() return("21F782AD-56A6-439D-B3D5-9A592F020E26")

Census_defaultkey <- function() return("b27cb41e46ffe3488af186dd80c64dce66bd5e87")

Census_url <- function() return("http://api.census.gov/data/eits/")

FRED_defaultkey <- function() return("c3cdbed96e921121504660952b0c6e0b")

