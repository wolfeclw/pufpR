
#' Impute missing GPS data on multiple files
#'
#' `batch_impute_coords()` imputes missing lon/lat coordinates that occur during
#' GPS lapses by sampling event.
#'
#' @param df an object created by `ufp_batch_read()`.
#' @param distance_threshold distance (meters) between the last known coordinates before
#' a GPS lapse and the first known coordinates after a lapse are compared to
#' this value.  If the distance exceeds this threshold, coordinates are not
#' imputed. Default = 100.
#' @param jitter_amount numeric; amount of jitter to apply to imputed coords.
#' Default = 0.00001 decimal degrees. See \code{\link[sf]{st_jitter}}.
#' @param show_lapse_distance logical; If TRUE, a column will display the distance (meters)
#' between the last known coordinate before a GPS lapse and first known coordinate
#' after a GPS lapse.  Default = FALSE.
#' @param fill_open_lapses logical; impute missing coordinates at the
#' beginning and end of the data frame (i.e. lapses not enclosed by known
#' coordinates). Default = FALSE.
#' @param speed_threshold criteria to impute open lapses. If the median speed
#' of cooridnates before or after an open lapse exceeds this threshold,
#' coordinates are not imputed.  Default = 5 (m/s).
#' @param speed_window numeric; number of rows used to calculate
#' "speed_threshold."
#' @param open_lapse_length if the number of rows in an open lapse exceed this
#' threshold, coordinates are not imputed. Default = 600.
#' @return a data frame.  A column is created to indicate whehter
#' coordinates were imputed ('imputed_coord'). The function also creates an
#' additional column stating the distance between lapses ('lapse_distance').
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_batch_impute(df,
#'   distance_threshold = 100, jitter_amount = 0.00001, show_lapse_distance = FALSE,
#'   fill_open_lapses = FALSE, speed_threshold = 5, speed_window = 60,
#'   open_lapse_length = 600
#' )
#' }
ufp_batch_impute <- function(df, distance_threshold = 100, jitter_amount = 0.00001, show_lapse_distance = FALSE,
                             fill_open_lapses = FALSE, speed_threshold = 5, speed_window = 60,
                             open_lapse_length = 600) {
  if (sum(stringr::str_detect(names(df), "Sampling_Event")) == 0) {
    stop("Column 'Sampling_Event' not found.")
  }

  impute_split <- df %>% split(., .$Sampling_Event)

  d_imputed <- if (fill_open_lapses == FALSE) {
    map_df(impute_split, ~ impute_coords_dist(.,
      distance_threshold = distance_threshold,
      jitter_amount = jitter_amount,
      show_lapse_distance = show_lapse_distance
    ))
  } else {
    map_df(impute_split, ~ impute_coords_open(.,
      distance_threshold = distance_threshold,
      jitter_amount = jitter_amount,
      show_lapse_distance = show_lapse_distance,
      speed_threshold = speed_threshold,
      speed_window = speed_window,
      open_lapse_length = open_lapse_length
    ))
  }

  if (sum(df$GPS_Valid) != nrow(df)) {
    n_imputed <- sum(d_imputed$imputed_coord, na.rm = TRUE)
    n_na_coords <- sum(is.na(d_imputed$lat))

    message(paste0(
      "A total of ", n_imputed, " (", scales::percent(n_imputed / nrow(d_imputed)), ")",
      " of the coordinates were imputed."
    ))
    message(paste0(
      "A total of ", n_na_coords, " (", scales::percent(n_na_coords / nrow(d_imputed)), ")",
      " of the coordinates are missing GPS data after imputation."
    ))
  }

  if (sum(df$GPS_Valid) == nrow(df)) {
    message("All location data is complete--coordinates were not imputed.")
  }

  d_imputed
}
