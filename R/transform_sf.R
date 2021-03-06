
#' Create `sf` Object
#'
#' Creates a multipoint sf object from a PUFP data frame created by `ufp_read()`
#' or `ufp_batch_read()`. Points are projected using WGS84 (crs = 4326).
#'
#' @param df data frame created by `ufp_read() `or `ufp_batch_read()`.  The input
#' data frame must include lon/lat coordinates.
#'
#' @return a `MULTIPOINT` object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_sf(df)
#' }
ufp_sf <- function(df) {
  if (is.data.frame(df) == FALSE) {
    stop("The input object is not a data frame.  Did you import data using the `ufp_read()` or `ufp_batch_read()` function?")
  }
  if (sum(is.na(df$lat)) == nrow(df)) {
    stop("The input data frame does not have any valid 'lon/lat' coordinates. The data frame cannont be converted to an `sf` object.")
  }
  if (sum(stringr::str_detect(names(df), "lon|lat")) < 1) {
    stop("The input data frame is missing longitude and latitude. Please set 'coords = TRUE' in `ufp_read()` or `batch_read_pufp()`.")
  }

  d_coords <- filter(df, !is.na(lat))
  d_coords_sf <- suppressWarnings(sf::st_as_sf(d_coords, coords = c("lon", "lat"), crs = 4326))
  d_join <- suppressMessages(full_join(df, d_coords_sf))
  df_sf <- sf::st_as_sf(d_join, crs = 4326)
  sf_empty_rows <- sum(sf::st_is_empty(df_sf))
  pct_empty <- round(sf_empty_rows / nrow(df_sf), digits = 2)

  if (pct_empty > .5) {
    warning(paste("Greater than 50% of the measurements in the input data frame have invalid GPS coordinates."),
      call. = FALSE
    )
  }
  return(df_sf)
}
