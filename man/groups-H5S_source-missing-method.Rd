% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5serv.R
\name{groups}
\alias{groups}
\alias{groups,H5S_source,missing-method}
\title{HDF5 server data groups accessor}
\usage{
groups(object, index, ...)
}
\arguments{
\item{object}{H5S_source instance}

\item{index}{numeric, if present, extracts metadata about selected group (sequential ordering 
of groups as returned by server) access for group information for HDF5 server}

\item{\dots}{not used}
}
\value{
a data frame with group name and number of links for each group
}
\description{
HDF5 server data groups accessor
}
\examples{
bigec2 = H5S_source("http://h5s.channingremotedata.org:5000")
groups(bigec2)
}
