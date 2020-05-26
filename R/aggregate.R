
#' Aggregate PUFP Time Series
#'
#' Aggregate PUFP time series by specified time unit
#'
#' @param df an object created by `ufp_read()` or `ufp_batch_read()`.
#' @param unit a character string specifying a time unit or a multiple of a
#' unit to be rounded to. \code{\link[lubridate]{floor_date}}
#' @param floor_or_celiling "floor" = `floor_date()`;
#' "ceiling" = `ceilig_date()`
#' @param summary_fun summary function (i.e. "mean", "median").
#' Default = "median."
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_aggregate(df,
#'   unit = "5 seconds", floor_or_ceiling = "floor",
#'   summary_fun = "median"
#' )
#' }
#' @importFrom stats median
ufp_aggregate <- function(df, unit = "5 seconds", floor_or_celiling = "floor",
                          summary_fun = "median") {
  if (sum(stringr::str_detect(names(df), "Date_Time")) == 0) {
    stop("`Date_Time` column not found.")
  }

  if (floor_or_celiling == "floor") {
    d_agg <- df %>%
      mutate(agg_dt = lubridate::floor_date(Date_Time, unit = unit)) %>%
      group_by(agg_dt) %>%
      summarise_if(is.numeric, summary_fun, na.rm = TRUE) %>%
      rename(Date_Time = agg_dt) %>%
      mutate(
        Date = lubridate::as_date(Date_Time),
        Time = hms::as_hms(Date_Time),
        GPS_Valid = ifelse(is.na(lat), 0, 1)
      ) %>%
      select(Date_Time, Date, Time, everything())
  } else {
    d_agg <- df %>%
      mutate(agg_dt = lubridate::ceiling_date(Date_Time, unit = unit)) %>%
      group_by(agg_dt) %>%
      summarise_if(is.numeric, summary_fun, na.rm = TRUE) %>%
      rename(Date_Time = agg_dt) %>%
      mutate(
        Date = lubridate::as_date(Date_Time),
        Time = hms::as_hms(Date_Time),
        GPS_Valid = ifelse(is.na(lat), 0, 1)
      ) %>%
      select(Date_Time, Date, Time, everything())
  }

  char_cols <- select_if(df, is.character) %>% names()
  rm_cols <- char_cols[!stringr::str_detect(char_cols, "Sensor")]

  if (length(rm_cols > 0)) {
    message(
      "Columns `", paste(rm_cols, collapse = ", "),
      "` are of class 'character' and were removed during aggregation."
    )
  }

  if (sum(stringr::str_detect(char_cols, "Sensor")) == 1) {
    d_agg %>%
      mutate(Sensor = unique(df$Sensor)) %>%
      select(Date_Time:Time, starts_with("UFP"), Sensor, everything())
  } else {
    d_agg
  }
}
