% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BEA_api.r
\name{NIPA}
\alias{NIPA}
\title{Query the BEA API to get a single variable}
\usage{
NIPA(var, freq, dsname, key)
}
\arguments{
\item{var}{a variable name in c("gdp", "gdppi", "rgdp")}

\item{freq}{"q" or "a"; default: "q"}

\item{dsname}{text name of the dataset (e.g., "NIPA") (see \code{BEA_DSlist}); default: "NIPA"}

\item{key}{Your BEA API key (can be obtained for free - check www.bea.gov); default: my key}
}
\value{
data frame with data
}
\description{
\code{NIPA} returns a data frame with requested regional data. Right now only works with annual.
}
\details{
Queries the BEA API to get the data.
}
\examples{
NIPA()
df <- NIPA("gdppi", "q")
head(df)
df <- NIPA("gdp", "a")
head(df)
df <- NIPA("rgdp")
head(df)
}
\keyword{NIPA}

