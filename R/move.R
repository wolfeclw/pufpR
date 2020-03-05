

### FUNCTION TO CALCULATE SPEED AND AZIMUTH OF CONSECUTIVE PUFP MEASUREMENTS

ufp_move <- function(df) {
  
  if (sum(stringr::str_detect(names(df), "lat")) == 0) {
    stop("No 'lat' or 'lon' colunms found.  Did you set 'coords = TRUE' when reading PUFP file?")
  }
  
  d_speed <- df %>% 
    filter(!duplicated(Date_Time)) 
  
  if (sum(str_detect(names(df), "Sampling_Event"))) {
    d_speed <- group_by(d_speed, Sampling_Event)
  }
  
  d_speed <- d_speed %>%
    mutate(lag_time = lag(lubridate::ymd_hms(Date_Time)),
           lag_time.diff = lubridate::ymd_hms(Date_Time) - lag(lubridate::ymd_hms(Date_Time)),
           lag_lat = lag(lat),
           lag_lon = lag(lon),
           distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag_lon, lag_lat),  r = 6378137),
           azimuth = geosphere::bearing(cbind(lon, lat), cbind(lag_lon, lag_lat)),
           speed_ms = round(distance / as.numeric(lag_time.diff), digits = 1),
           lag_speed_ms = lag(speed_ms),
           speed_inf = is.infinite(speed_ms),
           pufp_speed = ifelse(is.nan(speed_ms), NA,
                               ifelse(is.infinite(speed_ms), NA, speed_ms))) %>%
    select(-c(lag_time, lag_time.diff, lag_lat, lag_lon))
  
  message(paste("A total of", sum(duplicated(df$Date_Time)), "rows had dupliated timestamps and were removed."))
  
  d_speed
}
