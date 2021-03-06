% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circularize.R
\name{ufp_circularize}
\alias{ufp_circularize}
\title{Calcute circular vairance and clustering}
\usage{
ufp_circularize(
  df,
  circvar_threshold = 0.7,
  window = 60,
  cluster_threshold = NULL,
  show_circvar = FALSE
)
}
\arguments{
\item{df}{an object created by \verb{ufp_move().} The input data frame must include
'speed' and 'azimuth' before circular variance can be calcuated and clusters
can be identified.}

\item{circvar_threshold}{numeric; Threshold value to use to determine clustering.}

\item{window}{window (row) used to calculate circular variance.}

\item{cluster_threshold}{threshold value used to define the number of observations (rows)
in each cluster.  If the number of points in a cluster is less than this value,
they will not be defined as a cluster.}

\item{show_circvar}{Show circular variance (\code{circvar}) within the specified 'window.'
Default = 0.7.}
}
\value{
a tibble.
}
\description{
\code{ufp_circularize} calculates the circular variance of PUFP measurements and
includes arguements to define geographic clustering parameters. Imputing
lon/lat values using \code{ufp_impute()} or \code{batch_impute_coords()} is
recommended if GPS coordinates are missing.
}
\examples{
\dontrun{

ufp_circularize(df,
  circvar_threshold = .7, window = 60, cluster_threshold = NULL,
  show_circvar = FALSE
)
}
}
