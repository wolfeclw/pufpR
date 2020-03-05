

### batch import PUFP files

ufp_batch_read <- function(pufp_files, event_threshold = 10, parse_errors = FALSE, tz = "America/New_York",
                           truncate_ufp = TRUE, coords = TRUE) {
  d_pufp <- map(pufp_files, ~ read_pufp(.,
    parse_errors = parse_errors, tz = tz,
    truncate_ufp = truncate_ufp, coords = coords
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
