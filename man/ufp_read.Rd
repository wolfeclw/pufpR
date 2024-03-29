% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{ufp_read}
\alias{ufp_read}
\title{Read PUFP .txt file}
\usage{
ufp_read(
  path,
  tz = "America/New_York",
  truncate_ufp = TRUE,
  coords = TRUE,
  ufp_check = FALSE,
  participant_id = NULL,
  sample_col = NULL,
  sample_id = NULL,
  time_correction = NULL
)
}
\arguments{
\item{path}{a path.}

\item{tz}{a character string that specifies which time zone to parse the
date with. Default = 'America/New_York.'}

\item{truncate_ufp}{truncate UFP concentration? If TRUE (the default), UFP
concentrations above 250K will be right censored.}

\item{coords}{parse GPS string to derive latitude and longitude?
Default = TRUE.}

\item{ufp_check}{check for invalid UFP measurements.  If TRUE, new columns
named \code{UFP_NA} and \code{UFP_Invalid} are created to flag missing and potentially
invalid UFP concentrations.}

\item{participant_id}{user defined string to denote a personal identifier.
This is useful if the PUFP is deployed during personal sampling.  If specified,
a new column is created ('ID'). Default is NULL.}

\item{sample_col}{user defined character string specifying the name of the
column to denote sample ID. Default is NULL.}

\item{sample_id}{user defined string to denote sample ID. If assigned, a
value must also be supplied to \code{sample_col}. Default is NULL.}

\item{time_correction}{numeric; second threshold to correct \code{Date_Time} using GPS
recorded time.}
}
\value{
a tibble.
}
\description{
\code{ufp_read()} imports and cleans a PUFP text file.
}
\examples{
\dontrun{

ufp_read(path,
  tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE,
  ufp_check = FALSE, participant_id = NULL, sample_col = NULL, sample_id = NULL
)
}
}
