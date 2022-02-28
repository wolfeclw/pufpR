

#' Read multiple PUFP .txt files
#'
#' `ufp_batch_read()` imports and cleans multiple PUFP text files as a single
#' data frame.  Measurements from different monitoring sessions are denoted by
#' the 'Sampling_Event' column, which is defined by `event_threshold`.
#'
#' @param paths a vector of paths
#' @param event_threshold numeric; difftime value in minutes to differentiate
#' sampling events. Consecutive measurements that exceed this threshold are separated
#' into new sampling events. Default = 10.
#' @param tz a character string that specifies which time zone to parse the
#' date with. Default = 'America/New_York.'
#' @param truncate_ufp logical; truncate UFP concentration? If TRUE
#' (the default), UFP concentrations above 250K will be right censored.
#' @param coords logical; arse GPS string to derive latitude and longitude?
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
#' @return a tibble. Additional columns are created for 'Sampling_Event' and
#' 'Sampling_Day.'  Sampling events represent discreet intervals in sampling
#' defined by the `event_threshold`.  Sampling day is derived from the date
#' of the measurements.  Multiple sampling events may occur within a sampling
#' day.
#' @export
#' @examples
#' \dontrun{
#'
#' ufp_batch_read(c(path1, path2, path3),
#'   event_threshold = 10,
#'   tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE, ufp_check = FALSE,
#'   participant_id = NULL, sample_col = NULL, sample_id = NULL
#' )
#' }
ufp_batch_read <- function(paths, event_threshold = 10, tz = "America/New_York",
                           truncate_ufp = TRUE, coords = TRUE, ufp_check = FALSE,
                           participant_id = NULL, sample_col = NULL,
                           sample_id = NULL, time_correction = NULL) {
  d_pufp <- map(paths, ~ ufp_read(.,
                                  tz = tz, truncate_ufp = truncate_ufp,
                                  coords = coords, ufp_check = ufp_check,
                                  participant_id = participant_id,
                                  sample_col = sample_col, sample_id = sample_id
  )) %>%
    reduce(., bind_rows) %>%
    arrange(Date_Time) %>%
    mutate(
      lag_time = lag(Date_Time),
      Time_Diff.sec = abs(Date_Time - lag_time),
      break_yn = ifelse(is.na(Time_Diff.sec), 0,
                        ifelse(Time_Diff.sec > event_threshold * 60, 1, 0)
      ),
      Sampling_Event = cumsum(break_yn) + 1,
      Sampling_Day = as.numeric(Date - first(Date) + 1)
    ) %>%
    select(Date_Time:Time, Sampling_Day, Sampling_Event, everything()) %>%
    select(-c(lag_time, Time_Diff.sec, break_yn))
  
  if (!is.null(sample_col)) {
    d_pufp <- relocate(d_pufp, {{ sample_col }})
  }
  if (!is.null(participant_id)) {
    d_pufp <- relocate(d_pufp, ID)
  }
  
  if(!is.null(time_correction) & !is.numeric(time_correction)) {
    stop('`time correction` must assigned a numeric value.', call. = FALSE)
  } else if (!is.null(time_correction)) {
    d_gps_split <- d_pufp %>% 
      group_split(Sampling_Event) 
    
    d_gps_time <- d_gps_split %>% 
      map(., ~filter(., !is.na(lon))) %>% 
      map_if(., ~nrow(.) > 0, ~mutate(.,
                                      utc = lubridate::as_datetime(Date_Time, tz = 'UTC'),
                                      gps_split = str_split(GPS_Signal, ','),
                                      gps_time = map_chr(gps_split, 2),
                                      gps_hr = substr(gps_time, 1, 2),
                                      gps_min = substr(gps_time, 3, 4),
                                      gps_sec = substr(gps_time, 5, 6),
                                      gps_hms = hms::as_hms(str_c(gps_hr, gps_min, gps_sec, sep = ':')),
                                      date_utc = lubridate::as_date(utc),
                                      gps_dt = lubridate::as_datetime(paste(date_utc, gps_hms)),
                                      gps_date_time = lubridate::force_tzs(gps_dt, tzone = 'UTC', tzone_out  = 'America/New_York'),
                                      gps_diff = difftime(Date_Time, gps_date_time, units = 'min'))) 
    
    med_diff <- map(d_gps_time, ~median(.$gps_diff))

    c_function <- function(l_event, tdiff) {

      if (is.null(tdiff)) {
        message(crayon::red(paste0('Sampling Event ', l_event$Sampling_Event[1],
                                   ': Time correction not possible - no GPS data was recorded.')))
        l_event
      } else if (abs(tdiff) > time_correction) {
        message(crayon::red(paste0('Sampling Event ', l_event$Sampling_Event[1], ': The median time difference (',
                                   round(tdiff, digits = 1), ' mins) between the GPS signal and the PUFP reading was greater than \n the correction threshold (',
                                   time_correction, ' mins). `Date_Time` has been corrected.')))
        l_event$Date_Time <- l_event$Date_Time - tdiff

        l_event
      } else if (abs(tdiff) < time_correction) {
        message(crayon::green(paste0('Sampling Event ', l_event$Sampling_Event[1], ': The median time difference (',
                                     round(tdiff, digits = 1), ' mins) between the GPS signal and the PUFP reading was less than \n the correction threshold (',
                                     time_correction, ' mins). `Date_Time` was not corrected.')))
        l_event
      }
    }
    d_pufp <- map2_df(d_gps_split, med_diff, ~c_function(.x, .y))
  }
  d_pufp
  
}
