 

#'Circular Variance and Clustering
#' 
#' Calculate the circular variance of PUFP measurements and geographic clustering.
#' Imputing lon/lat using `ufp_impute()` or `batch_impute_coords()` is
#' recommended if GPS coordinates are missing.
#'
#' @param df an object created by `ufp_move().` The input data frame must include
#' 'speed' and 'azimuth' before circular variance can be calcuated and clusters 
#' can be identified.
#' @param circvar_threshold numeric; Threshold value to use to determine clustering.
#' @param window window (row) used to calculate circular variance.
#' @param cluster_threshold threshold value used to define the number of observations (rows)
#' in each cluster.  If the number of points in a cluster is less than this value, 
#' they will not be defined as a cluster.
#' @param show_circvar Show circular variance (`circvar`) within the specified 'window.'
#' Default = 0.7.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_circularize(df,
#'   circvar_threshold = .7, window = 60, cluster_threshold = NULL,
#'   show_circvar = FALSE
#' )
#' }
ufp_circularize <- function(df, circvar_threshold = .7, window = 60, cluster_threshold = NULL, show_circvar = FALSE) {
  if (sum(stringr::str_detect(names(df), "azimuth")) == 0) {
    stop("Column 'azimuth' not found.  Use `ufp_move()` to calculate the azimuth of the UFP measurements.",
      call. = FALSE
    )
  }
  if (sum(stringr::str_detect(names(df), "Sampling_Event"))) {
    d <- group_by(df, Sampling_Event)
  } else {
    d <- df
  }
  dt_diff <- d$Date_Time - lag(d$Date_Time)
  d_variance <- d %>%
    mutate(
      a_rad = circular::rad(azimuth),
      a_sin = sin(a_rad),
      a_cos = cos(a_rad),
      sum_sin = zoo::rollsum(a_sin, window, na.rm = TRUE, fill = NA, align = "center"),
      sum_sin = zoo::na.locf(sum_sin, na.rm = FALSE, maxgap = window),
      sum_cos = zoo::rollsum(a_cos, window, na.rm = TRUE, fill = NA, align = "center"),
      sum_cos = zoo::na.locf(sum_cos, na.rm = FALSE, maxgap = window / 2),
      a_y2 = (sum_sin / window)^2,
      a_x2 = (sum_cos / window)^2,
      r = ifelse(is.na(lat), NA, sqrt(a_y2 + a_x2)),
      circvar = round(1 - r, digits = 1),
      roll_speed = zoo::rollmedian(speed_ms, 12, na.rm = TRUE, fill = NA, align = "center"),
      roll_speed = zoo::na.locf(roll_speed, na.rm = FALSE, maxgap = 12),
      move_break = ifelse(circvar >= .7 & roll_speed < 2, 1, 0),
      rw_num = row_number()
    ) %>%
    select(-c(a_rad:r, roll_speed))

  if (sum(d_variance$move_break, na.rm = TRUE) > 0) {
    d_places <- ufp_places(d_variance)
    d_places$clustered_coord <- ifelse(is.na(d_places$place_grp), 0, 1)

    d_places <- d_places %>%
      ufp_place_lapse() %>%
      place_lapse_dist()

    if (max(d_places$place_grp, na.rm = TRUE) > 1) {
      d_places <- d_places %>%
        mutate(place_grp = ifelse(is.na(place_grp) & pl_distance < 100, zoo::na.locf(place_grp, na.rm = FALSE),
          place_grp
        ))
    }

    d_clusters <- ufp_cluster(d_places, cluster_threshold = cluster_threshold)

    if (is.na(d_clusters$cluster_grp[window / 2 - 1])) {
      d_clusters$cluster_grp <- zoo::na.locf(d_clusters$cluster_grp,
        fromLast = TRUE,
        na.rm = FALSE, maxgap = window / 2
      )
    }
  } else if (sum(!is.na(df$lat) > 0) & sum(d_variance$move_break, na.rm = TRUE) == 0) {
    d_clusters <- d_variance %>%
      select(-c(move_break, rw_num)) %>%
      mutate(cluster_grp = NA)
    message(paste0(
      "NO CLUSTERS IDENTIFIED - the participant may have been in transit",
      "\n for the duration of the sampling period."
    ))
  } else if (sum(is.na(df$lat)) == nrow(df)) {
    d_clusters <- df
    message(paste0(
      "INVALID INPUT DATA - the input data frame does not have valid 'lon/lat' coordinates.",
      "\n Data unable to be clustered."
    ))
  }

  if (show_circvar == TRUE & sum(is.na(df$lat)) != nrow(df)) {
    d_clusters <- d_clusters
  } else if (show_circvar == FALSE & sum(is.na(df$lat)) != nrow(df)) {
    d_clusters <- d_clusters %>% select(-circvar)
  } else if (sum(is.na(df$lat)) == nrow(df)) {
    d_clusters <- d_clusters %>% mutate(
      circvar = NA,
      cluster_grp = NA
    )
  }
  d_clusters
}
