% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5serv.R
\name{getDims}
\alias{getDims}
\title{getDims from hsds server}
\usage{
getDims(object)
}
\arguments{
\item{object}{instance of H5S_source(updated object with path to file set)}
}
\value{
numeric content of dimensions
}
\description{
getDims from hsds server
}
\examples{
hsdsCon = H5S_source("http://149.165.156.12:5101") # hsds server
setPath(hsdsCon, "/home/reshg/tenx_full2.h5")-> hsds
getDims(hsds)
}
