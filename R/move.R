
#' Calculate speed and azimuth of PUFP measurements
#'
#' `ufp_move()` calculates the speed and azimuth (bearing) of consecutive PUFP 
#' measurements. If multiple sampling events are included in the input file, the
#' data frame is grouped and speed and azimuth are calcuated by event.
#'
#' @param df data frame created by `ufp_read() `or `ufp_batch_read()`
#'
#' @return a data frame.  Speed is expressed in meters per second; azimuth is
#' expressed in degrees.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_move(df)
#' }
ufp_move <- function(df) {
  if (sum(stringr::str_detect(names(df), "lat")) == 0) {
    stop("No 'lat' or 'lon' colunms found.  Did you set 'coords = TRUE' when reading PUFP file?")
  }

  if (sum(is.na(df$lat)) == nrow(df)) {
    message("The input data frame does not have valid 'lon/lat' coordinates. Speed and azimuth were not calculated.")
  }

  d_speed <- df %>%
    filter(!duplicated(Date_Time)) %>%
    arrange(Date_Time) # arrange by Date_Time just to be safe

  if (sum(stringr::str_detect(names(df), "Sampling_Event"))) {
    d_speed <- group_by(d_speed, Sampling_Event)
  }

  d_speed <- d_speed %>%
    mutate(
      lag_time = lag(lubridate::ymd_hms(Date_Time)),
      lag_time.diff = lubridate::ymd_hms(Date_Time) - lag(lubridate::ymd_hms(Date_Time)),
      lag_lat = lag(lat),
      lag_lon = lag(lon),
      distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag_lon, lag_lat)),
      azimuth = geosphere::bearing(cbind(lon, lat), cbind(lag_lon, lag_lat)) + 180,
      speed_ms = round(distance / as.numeric(lag_time.diff), digits = 1),
      lag_speed_ms = lag(speed_ms),
      speed_inf = is.infinite(speed_ms),
      speed_ms = ifelse(is.nan(speed_ms), NA,
        ifelse(is.infinite(speed_ms), NA, speed_ms)
      )
    ) %>%
    select(-c(starts_with("lag"), speed_inf, distance))

  message(paste("A total of", sum(duplicated(df$Date_Time)), "rows had dupliated timestamps and were removed."))

  d_speed
}
