% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_sf.R
\name{ufp_sf}
\alias{ufp_sf}
\title{Create \code{sf} Object}
\usage{
ufp_sf(df)
}
\arguments{
\item{df}{data frame created by \code{ufp_read() }or \code{ufp_batch_read()}.  The input
data frame must include lon/lat coordinates.}
}
\value{
a \code{MULTIPOINT} object
}
\description{
Creates a multipoint sf object from a PUFP data frame created by \code{ufp_read()}
or \code{ufp_batch_read()}. Points are projected using WGS84 (crs = 4326).
}
\examples{
\dontrun{

ufp_sf(df)
}
}
