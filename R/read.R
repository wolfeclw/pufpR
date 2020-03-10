
#' Read PUFP .txt File
#'
#'Reads and cleans text file output from PUFP sensor.
#'
#' @param path a path.
#' @param tz a character string that specifies which time zone to parse the 
#' date with. Default = "America/New_York."
#' @param truncate_ufp truncate UFP concentration? If TRUE (the default), UFP
#' concentrations above 250K will be right censored.
#' @param coords parse GPS string to derive latitude and longitude? 
#' Default = TRUE.
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ufp_read(path, tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE)
#' }
#' @importFrom purrr map map_chr map_df map_dbl reduce
#' @import dplyr

ufp_read <- function(path, tz = "America/New_York", truncate_ufp = TRUE, coords = TRUE) {
  pufp_df <- geo_pufp(path, tz = tz, truncate_ufp = truncate_ufp, coords = coords)

  pufp_df
}
