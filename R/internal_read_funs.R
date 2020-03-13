
###### INTERNAL READ FUNCTIONS #####

import_pufp <- function(path) {
  readr::read_tsv(path,
                  skip = 1, trim_ws = TRUE, col_names = FALSE,
                  col_types = readr::cols(
                    .default = readr::col_character(),
                    X2 = readr::col_time(),
                    X3 = readr::col_double()
                  )
  )
}

cols_pufp <- function(path) {
  d <- suppressWarnings(import_pufp(path))
  
  d_names <- c("Date", "Time", "UFP_conc", "GPS_status", "GPS_Signal", "na_col", "Sensor")
  d <- stats::setNames(d, d_names)
  
  if (length(d) > 7) {
    d <- suppressMessages(as_tibble(d, .name_repair = "unique")) %>%
      rename(Warning = 8)
  } else {
    d <- mutate(d, Warning = NA)
  }
  
  d <- select(d, -na_col)
  d
}

clean_pufp <- function(path, tz = "America/New_York", truncate_ufp = TRUE) {
  d_cols <- cols_pufp(path)
  d_cols$Date <- lubridate::mdy(d_cols$Date)
  d_cols$Date_Time <- lubridate::ymd_hms(paste(d_cols$Date, d_cols$Time), tz = tz)
  d_cols <- arrange(d_cols, Date_Time)
  
  if (truncate_ufp == TRUE) {
    d_cols$UFP_conc250 <- ifelse(d_cols$UFP_conc > 2.5e5, 2.5e5, d_cols$UFP_conc)
  }
  
  d_clean <- d_cols %>%
    mutate(gps_element_count = stringr::str_count(GPS_Signal, ",")) %>%
    filter(gps_element_count >= 5) %>%
    select(Date_Time, Date, Time, starts_with("UFP"), everything()) %>%
    select(-gps_element_count)
  
  d_clean
}


geo_pufp <- function(path, tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE) {
  clean_df <- clean_pufp(path, tz = tz, truncate_ufp = truncate_ufp)
  
  if (coords == FALSE) {
    geo_df <- clean_df
  } else {
    lat_lon_df <- clean_df %>%
      mutate(
        GPS_split = stringr::str_split(GPS_Signal, pattern = ","),
        latitude = map_chr(GPS_split, 3),
        longitude = map_chr(GPS_split, 5),
        GPS_Valid = ifelse(nchar(latitude) > 2, 1, 0),
        deg_lat = as.numeric(substr(latitude, start = 1, stop = 2)),
        mm_lat = as.numeric(substr(latitude, start = 3, stop = 10)),
        deg_lon = as.numeric(substr(longitude, start = 1, stop = 3)),
        mm_lon = as.numeric(substr(longitude, start = 4, stop = 11)),
        ds_lat = mm_lat / 60,
        ds_lon = mm_lon / 60,
        lat = deg_lat + ds_lat,
        lon = (deg_lon + ds_lon) * -1
      ) %>%
      select(GPS_Valid, lat, lon)
    
    geo_df <- bind_cols(clean_df, lat_lon_df)
    
    empty_GPS <- sum(is.na(geo_df$lat))
    pct_empty <- round(empty_GPS / nrow(geo_df), digits = 2)
    
    message(paste(
      "A total of", empty_GPS, "rows or", scales::percent(pct_empty),
      "of the data in PUFP file", deparse(path), "is missing lat/lon coordinates."
    ))
  }
  geo_df
}


