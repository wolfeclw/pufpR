
#' Read PUFP .txt file
#'
#' `ufp_read()` imports and cleans a PUFP text file.
#'
#' @param path a path.
#' @param tz a character string that specifies which time zone to parse the
#' date with. Default = 'America/New_York.'
#' @param truncate_ufp truncate UFP concentration? If TRUE (the default), UFP
#' concentrations above 250K will be right censored.
#' @param coords parse GPS string to derive latitude and longitude?
#' Default = TRUE.
#' @param ufp_check check for invalid UFP measurements.  If TRUE, new columns
#' named `UFP_NA` and `UFP_Invalid` are created to flag missing and potentially 
#' invalid UFP concentrations.  
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_read(path, tz = 'America/New_York', truncate_ufp = TRUE, coords = TRUE)
#' }
#' @importFrom purrr map map_chr map_df map_dbl reduce
#' @importFrom pkgcond suppress_conditions
#' @import dplyr

ufp_read <- function(path, tz = 'America/New_York', truncate_ufp = TRUE, coords = TRUE,
                     ufp_check = FALSE) {
  pufp_df <- geo_pufp(path, tz = tz, truncate_ufp = truncate_ufp, coords = coords,
                      ufp_check = FALSE)

  pufp_df
}
