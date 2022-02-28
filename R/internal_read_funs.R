
###### INTERNAL READ FUNCTIONS #####


## import .txt file

import_pufp <- function(path) {
  options(warn = -1)
  d <- pkgcond::suppress_conditions(readr::read_tsv(path,
    skip = 1, trim_ws = TRUE,
    col_names = c(
      "Date", "Time", "UFP_conc", "GPS_status",
      "GPS_Signal", "na_col", "Sensor", "Warning"
    )
  ), class = c("warning", "message"))

  d %>%
    select(-na_col)
}


## truncate UFP values and check validity

clean_pufp <- function(path, tz = "America/New_York", truncate_ufp = TRUE, ufp_check = FALSE) {
  d_cols <- import_pufp(path)
  d_cols$Date <- lubridate::mdy(d_cols$Date)
  d_cols$Date_Time <- lubridate::ymd_hms(paste(d_cols$Date, d_cols$Time), tz = tz)
  d_cols <- arrange(d_cols, Date_Time)

  if (truncate_ufp == TRUE) {
    d_cols$UFP_conc250 <- ifelse(d_cols$UFP_conc > 2.5e5, 2.5e5, d_cols$UFP_conc)
  }

  if (ufp_check == TRUE) {
    d_cols <- d_cols %>%
      mutate(
        UFP_NA = if_else(is.na(UFP_conc), 1, 0),
        med_roll = zoo::rollmedian(UFP_conc, 31, fill = NA),
        new_med = ifelse(is.na(med_roll), UFP_conc, med_roll),
        LT_TEN = ifelse(new_med < 10, TRUE, FALSE),
        UFP_Valid = ifelse(UFP_NA == 1 | LT_TEN == TRUE, 0, 1)
      ) %>%
      select(-c(med_roll, new_med, LT_TEN))
  }

  d_clean <- d_cols %>%
    mutate(gps_element_count = stringr::str_count(GPS_Signal, ",")) %>%
    filter(gps_element_count >= 5) %>%
    select(Date_Time, Date, Time, starts_with("UFP"), everything()) %>%
    select(-gps_element_count)

  d_clean
}


## parse GPS signal to lon/lat coordinates

geo_pufp <- function(path, tz = "America/New_York", truncate_ufp = TRUE, ufp_check = FALSE,
                     coords = TRUE) {
  clean_df <- clean_pufp(path, tz = tz, truncate_ufp = truncate_ufp, ufp_check = ufp_check)

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


## tag UFP file with sampling session info

tag_pufp <- function(path, tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE,
                     ufp_check = FALSE, participant_id = NULL, sample_col = NULL,
                     sample_id = NULL) {
  geo_df <- geo_pufp(path,
    tz = tz, truncate_ufp = truncate_ufp, coords = coords,
    ufp_check = ufp_check
  )

  if (!is.null(sample_col) & !is.character(sample_col)) {
    stop("`sample_col` must be a character string.",
      call. = FALSE
    )
  }

  if (sum(is.null(sample_col), is.null(sample_id)) == 1) {
    stop("Both `sample_col` and `sample_id` must be assigned a value, not one or the other.",
      call. = FALSE
    )
  } else if (sum(is.null(sample_col), is.null(sample_id)) == 0) {
    tag_df <- mutate(geo_df, {{ sample_col }} := sample_id) %>%
      relocate({{ sample_col }})
  } else {
    tag_df <- geo_df
  }

  if (!is.null(participant_id)) {
    tag_df <- mutate(tag_df, ID = participant_id) %>%
      relocate(ID)
  }
  tag_df
}
