
### INTERNAL CIRCULARIZE FUNCTIONS

# `ufp_places` defines/assigns numeric category to places identified by the 
# circular variance algorithm

ufp_places <- function(df) {
  
  d_places <- df %>%
    filter(move_break == 1) %>%
    mutate(
      lag_rownum = lag(rw_num),
      rw_diff = rw_num - lag_rownum,
      place_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
      place_grp = cumsum(place_break)
    )  %>%
    select(-c(move_break, lag_rownum, rw_diff, place_break))
  
  d_places <- full_join(df, d_places)
}

# `ufp_lapse` identifies lapses between places

ufp_place_lapse <- function(df) {
  
  rm_open_na_cluster <- df %>%
    filter(
      cumsum(clustered_coord) != 0,
      rev(cumsum(rev(clustered_coord)) != 0)
    )
  
  if (nrow(rm_open_na_cluster) > 0) {
    d_place_lapse <- rm_open_na_cluster %>%
      mutate(place_lapse = ifelse(clustered_coord == 0, 1, 0)) %>% 
      filter(place_lapse == 1)
    
    d_place_lapse <- d_place_lapse %>% 
      mutate(
        lag_rownum = lag(rw_num),
        rw_diff = rw_num - lag_rownum,
        break_yn = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
        place_lapse_grp = cumsum(break_yn)
      ) %>%
      select(-c(place_lapse, lag_rownum, rw_diff, break_yn))
    
    c_lapse_join <- full_join(df, d_place_lapse)
  }
}

# `place_lapse_dist` calculates distances between the centroids of sequential  
# places

place_lapse_dist <- function(df) {
  
  p_lapse_grps <- df %>% 
    filter(!is.na(place_grp))
  
  p_lapse_grps <- p_lapse_grps %>%
    select(place_grp, lat, lon) %>%
    group_by(place_grp) %>%
    summarise(mlat = mean(lat),
              mlon = mean(lon))
  
  if (nrow(p_lapse_grps) > 1) {
    
    p_lapse1 <- p_lapse_grps[-nrow(p_lapse_grps), ] %>%
      sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)

    p_lapse2 <- p_lapse_grps[2:nrow(p_lapse_grps), ] %>%
      sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)

    p_dist <- sf::st_distance(p_lapse1, p_lapse2, by_element = TRUE) %>%
      enframe(name = NULL) %>%
      mutate(place_lapse_grp = 1:nrow(.),
             pl_distance = as.numeric(round(value, digits = 3))) %>%
      select(place_lapse_grp,
             pl_distance)

    p_dist_join <- full_join(df, p_dist)
  } else {
    df
  }
}

# `ufp_cluster` aggregates places identified by the circular variance algorithm
# into larger clusters

ufp_cluster <- function(df, cluster_threshold = NULL) {
  
  d_places <- df %>%
    filter(!is.na(place_grp)) %>%
    mutate(
      lag_rownum = lag(rw_num),
      rw_diff = rw_num - lag_rownum,
      clust_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
      cluster_grp = cumsum(clust_break)
    )  %>%
    select(-c(clust_break, lag_rownum, rw_diff, clust_break))
  
  clust_join <- full_join(df, d_places)
  
  d_clust <- clust_join %>% 
    group_by(cluster_grp) %>%
    mutate(cluster_n = ifelse(is.na(cluster_grp), NA, n())) %>% 
    ungroup()
  
  d_clust[!is.na(d_clust$cluster_n) & d_clust$cluster_n < 20, "cluster_grp"] <- NA 
  
  d_clust
}


  
  






  
  
  
  
