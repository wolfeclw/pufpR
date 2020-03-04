

### FUNCTION TO CALCULATE TIME WINDOW AVERAGES/MEDIANS FOR UFP AND LAT/LON COORDINATES

aggregate_pufp <- function(df, interval = 5, interval_unit = "second", floor_or_celiling = "floor", 
                           summary_fun = "median") {
  
  if (sum(stringr::str_detect(names(df), "Date_Time")) == 0) {
    stop("`Date_Time` column not found.")
  }
  
  if (floor_or_celiling == "floor") {
    
  d_agg <- df %>% 
    mutate(agg_dt = floor_date(Date_Time, paste(interval, interval_unit))) %>% 
    group_by(agg_dt) %>% 
    summarise_if(is.numeric, summary_fun, na.rm = TRUE) %>% 
    rename(Date_Time = agg_dt) %>% 
    mutate(Date = lubridate::as_date(Date_Time),
           Time = hms::as_hms(Date_Time),
           GPS_Valid = ceiling(GPS_Valid)) %>% 
    select(Date_Time, Date, Time, everything())
  } else {
    d_agg <- df %>% 
      mutate(agg_dt = ceiling_date(Date_Time, paste(interval, interval_unit))) %>% 
      group_by(agg_dt) %>% 
      summarise_if(is.numeric, summary_fun, na.rm = TRUE) %>% 
      rename(Date_Time = agg_dt) %>% 
      mutate(Date = lubridate::as_date(Date_Time),
             Time = hms::as_hms(Date_Time),
             GPS_Valid = ceiling(GPS_Valid)) %>% 
      select(Date_Time, Date, Time, everything())
  }
  
  d_agg
  
}

