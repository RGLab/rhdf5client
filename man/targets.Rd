% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5serv.R
\name{targets}
\alias{targets}
\title{provide the full URLs for link members}
\usage{
targets(h5linkset, index)
}
\arguments{
\item{h5linkset}{instance of H5S_linkset}

\item{index}{numeric index into link vector - ignored}
}
\value{
a vector of dataset tags
}
\description{
provide the full URLs for link members
}
\examples{
bigec2 = H5S_source("http://h5s.channingremotedata.org:5000")
lks <- links(bigec2, 1)    # linkset for first group (Note: first group is the root group, by construction)
urls <- targets(lks)       # URLs of datasets in linkset
}
