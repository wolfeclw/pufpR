% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_coords.R
\name{ufp_impute}
\alias{ufp_impute}
\title{Impute missing GPS data}
\usage{
ufp_impute(
  df,
  distance_threshold = 100,
  jitter_amount = 1e-05,
  show_lapse_distance = FALSE,
  fill_open_lapses = FALSE,
  speed_threshold = 5,
  speed_window = 60,
  open_lapse_length = 600
)
}
\arguments{
\item{df}{data frame created by \code{ufp_read()}.}

\item{distance_threshold}{distance (meters) between the last known coordinates before
a GPS lapse and the first known coordinates after a lapse are compared to
this value.  If the distance exceeds this threshold, coordinates are not
imputed. Default = 100.}

\item{jitter_amount}{numeric; amount of jitter to apply to imputed coords.
Default = 0.00001 decimal degrees. See \code{\link[sf]{st_jitter}}.}

\item{show_lapse_distance}{NEEDS A DEF}

\item{fill_open_lapses}{logical; impute missing coordinates at the
beginning and end of the data frame (i.e. lapses not enclosed by known
coordinates). Default = FALSE.}

\item{speed_threshold}{criteria to impute open lapses. If the median speed
of cooridnates before or after an open lapse exceeds this threshold,
coordinates are not imputed.  Default = 5 (m/s).}

\item{speed_window}{numeric; number of rows used to calculate
'speed_threshold.'}

\item{open_lapse_length}{if the number of rows in an open lapse exceed this
threshold, coordinates are not imputed. Default = 600.}
}
\value{
a data frame.  An additional column is created to indicate whehter
coordinates were imputed ('imputed_coord'). he function also creates an
additional column stating the distance between lapses ('lapse_distance').
}
\description{
\code{ufp_impute()} imputes missing lon/lat coordinates that occur during GPS lapses.
}
\examples{
\dontrun{

ufp_impute(df,
  distance_threshold = 100, jitter_amount = 0.00001, fill_open_lapses = FALSE,
  speed_threshold = 5, speed_window = 60, open_lapse_length = 600
)
}
}
