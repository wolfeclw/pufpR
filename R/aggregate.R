
#' Aggregate PUFP data
#'
#' `ufp_aggregate()` condenses PUFP time series data to a specified time unit.
#'
#' @param df an object created by `ufp_read()` or `ufp_batch_read()`.
#' @param unit a character string specifying a time unit or a multiple of a
#' unit to be rounded
#' @param floor_or_celiling either 'floor'(\code{\link[lubridate]{floor_date}})
#' or 'ceiling'(\code{\link[lubridate]{ceiling_date}}). Default = 'floor.'
#' @param summary_fun summary function (i.e. 'mean', 'median').
#' Default = 'median.'
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
#' @importFrom purrr map map_chr map_lgl
#' 
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

  char_cols <- select_if(df, is.character) 
  char_lgl <- char_cols %>%
    map(., unique) %>%
    map_chr(., length) %>%
    map_lgl(., ~. == 1)

  char_keep <- char_cols[char_lgl][1:nrow(d_agg), ]
  rm_cols <- char_cols[!char_lgl] %>% names()

  if (length(rm_cols > 0)) {
    message(
      "Columns `", paste(rm_cols, collapse = ", "),
      "` are of class 'character' and were removed during aggregation."
    )
  }

  d_agg_char <- bind_cols(char_keep, d_agg)
  d_agg_char <- relocate(d_agg_char, Sensor, .after = last_col())

  d_agg_char
}
