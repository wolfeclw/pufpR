

### impute coords for batch data frame (i.e. file with sampling events)

ufp_batch_impute <- function(df, distance_threshold = 100, jitter_amount = 0.00001, speed_threshold = 5, 
                                speed_window = 60, fill_open_lapses = TRUE) {
  
  pute_batch <- function(df, distance_threshold = 100, jitter_amount = 0.00001, speed_threshold = 5, 
                         speed_window = 60, fill_open_lapses = TRUE) {
    
    d_imputed <- if (sum(df$GPS_Valid) == nrow(df)) {
      df
    } else if (fill_open_lapses == TRUE) {
      suppressMessages(impute_coords_open(df, distance_threshold = distance_threshold, jitter_amount = jitter_amount,
                                          speed_threshold = speed_threshold, speed_window = speed_window))
    } else {
      suppressMessages(impute_coords_dist(df, distance_threshold = distance_threshold, jitter_amount = jitter_amount))
    }
    
  }
  
  impute_split <- df %>% split(., .$Sampling_Event)
  
  d_imputed <- map(impute_split, ~pute_batch(., distance_threshold = distance_threshold, 
                                             jitter_amount = jitter_amount, speed_threshold = speed_threshold,
                                             speed_window = speed_window, fill_open_lapses = fill_open_lapses)) %>%
    reduce(., rbind)
  
  if (sum(df$GPS_Valid) != nrow(df)) {
    n_imputed <- sum(d_imputed$imputed_coord, na.rm = TRUE)
    n_na_coords <- sum(is.na(d_imputed$lat))
  }
  
  if (sum(df$GPS_Valid) == nrow(df)) {
    message("All location data is complete--coordinates were not imputed.")
  }
  
  message(paste0("A total of ", n_imputed , " (", scales::percent(n_imputed/nrow(d_imputed), accuracy = 0.1), ")",
                 " of the coordinates were imputed."))
  message(paste0("A total of ", n_na_coords, " (", scales::percent(n_na_coords/nrow(d_imputed), accuracy = 0.1), ")",
                 " of the coordinates are missing GPS data after imputation."))
  
  d_imputed
}
