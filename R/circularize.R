


###

ufp_circularize <- function(df, circvar_threshold = .7, window = 60, cluster_threshold = NULL) {
  
  if (sum(stringr::str_detect(names(df), "Sampling_Event"))) {
    d <- group_by(df, Sampling_Event)
  } else {
    d <- df
  }
  dt_diff <- d$Date_Time - lag(d$Date_Time)
  d_variance <- d %>%
    mutate(a_rad = circular::rad(azimuth),
           a_sin = sin(a_rad),
           a_cos = cos(a_rad),
           sum_sin = zoo::rollsum(a_sin, window, na.rm = TRUE, fill = NA, align = "center"),
           sum_sin = zoo::na.locf(sum_sin, na.rm = FALSE, maxgap = window),
           sum_cos = zoo::rollsum(a_cos, window, na.rm = TRUE, fill = NA, align = "center"),
           sum_cos = zoo::na.locf(sum_cos, na.rm = FALSE, maxgap = window/2),
           a_y2 = (sum_sin/window)^2,
           a_x2 = (sum_cos/window)^2,
           r = ifelse(is.na(lat), NA, sqrt(a_y2 + a_x2)),
           circvar = round(1 - r, digits = 1),
           roll_speed = zoo::rollmedian(speed_ms, 12, na.rm = TRUE, fill = NA, align = "center"),
           roll_speed = zoo::na.locf(roll_speed, na.rm = FALSE, maxgap = 12),
           move_break = ifelse(circvar >= circvar_threshold & roll_speed < 2, 1, 0),
           rw_num = row_number()) %>%
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
                                  place_grp))
    } 
    
    d_clusters <- ufp_cluster(d_places)
    
    # if (!is.null(cluster_threshold)) {
    #   d_places <- d_places %>% 
    #     group_by(place_grp) %>%
    #     mutate(place_n = ifelse(is.na(place_grp), NA, n())) %>% 
    #     ungroup()
    #   
    #   d_places[!is.na(d_places$place_n) & d_places$place_n < cluster_threshold, "place_grp"] <- NA 
    # }

    if (is.na(d_clusters$cluster_grp[window/2 - 1])) {
      d_clusters$cluster_grp <- zoo::na.locf(d_clusters$cluster_grp, fromLast = TRUE,
                                             na.rm = FALSE, maxgap = window/2)
    }
    
    
  }
  d_clusters
}

####



#   