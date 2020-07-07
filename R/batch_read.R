

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
#'
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
#'   tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE
#' )
#' }
ufp_batch_read <- function(paths, event_threshold = 10, tz = "America/New_York",
                           truncate_ufp = TRUE, coords = TRUE, ufp_check = FALSE) {
  d_pufp <- map(paths, ~ ufp_read(.,
    tz = tz, truncate_ufp = truncate_ufp,
    coords = coords, ufp_check = ufp_check
  )) %>%
    reduce(., rbind) %>%
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

  d_pufp
}
