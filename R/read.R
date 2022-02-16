
#' Read PUFP .txt file
#'
#' `ufp_read()` imports and cleans a PUFP text file.
#'
#' @param path a path.
#' @param tz a character string that specifies which time zone to parse the
#' date with. Default = 'America/New_York.'
#' @param truncate_ufp truncate UFP concentration? If TRUE (the default), UFP
#' concentrations above 250K will be right censored.
#' @param coords parse GPS string to derive latitude and longitude?
#' Default = TRUE.
#' @param ufp_check check for invalid UFP measurements.  If TRUE, new columns
#' named `UFP_NA` and `UFP_Invalid` are created to flag missing and potentially
#' invalid UFP concentrations.
#' @param participant_id  user defined string to denote a personal identifier.
#' This is useful if the PUFP is deployed during personal sampling.  If specified,
#' a new column is created ('ID'). Default is NULL.
#' @param sample_col user defined character string specifying the name of the
#' column to denote sample ID. Default is NULL.
#' @param sample_id user defined string to denote sample ID. If assigned, a
#' value must also be supplied to `sample_col`. Default is NULL.
#' @param time_correction numeric; second threshold to correct `Date_Time` using GPS 
#' recorded time. 
#' 
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_read(path,
#'   tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE,
#'   ufp_check = FALSE, participant_id = NULL, sample_col = NULL, sample_id = NULL
#' )
#' }
#' @importFrom purrr map map_chr map_df map_dbl reduce
#' @importFrom pkgcond suppress_conditions
#' @import dplyr

ufp_read <- function(path, tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE,
                     ufp_check = FALSE, participant_id = NULL, sample_col = NULL,
                     sample_id = NULL, time_correction = NULL) {
  pufp_df <- tag_pufp(path,
                      tz = tz, truncate_ufp = truncate_ufp, coords = coords,
                      ufp_check = ufp_check, participant_id = participant_id,
                      sample_col = sample_col, sample_id = sample_id
  )
  
  if(!is.null(time_correction) & sum(pufp_df$GPS_Valid) == 0) {
    message('Time correction not possible--no GPS data was recorded.')
  } else if (!is.null(time_correction)) {
    d_gps_time <- pufp_df %>% 
      mutate(utc = lubridate::as_datetime(Date_Time, tz = 'UTC'),
             gps_split = str_split(GPS_Signal, ','),
             gps_time = map_chr(gps_split, 2),
             gps_hr = substr(gps_time, 1, 2),
             gps_min = substr(gps_time, 3, 4),
             gps_sec = substr(gps_time, 5, 6),
             gps_hms = hms::as_hms(str_c(gps_hr, gps_min, gps_sec, sep = ':')),
             date_utc = lubridate::as_date(utc),
             gps_dt = lubridate::as_datetime(paste(date_utc, gps_hms)),
             gps_date_time = lubridate::force_tzs(gps_dt, tzone = 'UTC', tzone_out  = 'America/New_York'),
             gps_diff = difftime(Date_Time, gps_date_time, units = 'min')) 
    
    med_diff <- median(d_gps_time$gps_diff)
    
    if (med_diff > time_correction) {
      message(crayon::green(paste0('The mean time difference (',
                                   round(med_diff, digits = 1), ' mins) between the GPS signal and the PUFP reading was greater than the correction threshold (',
                                   time_correction, ' mins). `Date_Time` has been corrected.')))
      pufp_df$Date_Time <- pufp_df$Date_Time - med_diff
    }
  }
  
  pufp_df
}
