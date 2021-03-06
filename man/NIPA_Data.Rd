% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BEA_api.r
\name{NIPA_Data}
\alias{NIPA_Data}
\title{Get Data from a NIPA Table or a NIPA Underlying Detail Table (NIUnderlyingDetail)}
\usage{
NIPA_Data(tableid, freq, dsname, key, verbose)
}
\arguments{
\item{tableid}{the BEA table identifier (see \code{BEA_ParamVals})}

\item{freq}{"q" or "a"}

\item{dsname}{text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist})}

\item{key}{Your BEA API key (can be obtained for free - check www.bea.gov)}

\item{verbose}{default is FALSE}
}
\value{
data frame with data
}
\description{
\code{NIPA_Data} returns a data frame with a list of parameters for the particular dataset
Pay particular attention to whether a data set will accept multiple table flags, or whether tables must be
retrieved one by one
}
\details{
Queries the BEA API to get a NIPA table
}
\examples{
# quarterly gdp percent change NIPA TableID 1 ####
head(NIPA_Data(1, "q"))
require(dplyr)
gdppch <- NIPA_Data(1, "q") \%>\% # NIPA is default dsname so don't have to specify it
  filter(SeriesCode=="A191RL") \%>\%
  select(date, gdppch=value)
head(gdppch)
}
\keyword{NIPA_Data}

