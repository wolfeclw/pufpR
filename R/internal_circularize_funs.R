
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
    ) %>%
    select(-c(move_break, lag_rownum, rw_diff, place_break))

  d_places <- suppressMessages(full_join(df, d_places))
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

    c_lapse_join <- suppressMessages(full_join(df, d_place_lapse))
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
    summarise(
      mlat = mean(lat),
      mlon = mean(lon)
    )

  if (nrow(p_lapse_grps) > 1) {
    p_lapse1 <- p_lapse_grps[-nrow(p_lapse_grps), ] %>%
      sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)

    p_lapse2 <- p_lapse_grps[2:nrow(p_lapse_grps), ] %>%
      sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)

    p_dist <- sf::st_distance(p_lapse1, p_lapse2, by_element = TRUE) %>%
      tibble::enframe(name = NULL) %>%
      mutate(
        place_lapse_grp = 1:nrow(.),
        pl_distance = as.numeric(round(value, digits = 3))
      ) %>%
      select(
        place_lapse_grp,
        pl_distance
      )

    p_dist_join <- suppressMessages(full_join(df, p_dist))
  } else {
    df %>% mutate(pl_distance = NA)
  }
}

# `ufp_cluster` aggregates places identified by the circular variance algorithm
# into larger clusters. If the number of obs in a cluster is below the
# 'cluster_threshold', those observations are retained, but unclustered.
# Clusters are reordered if any observations are unclustered.

ufp_cluster <- function(df, cluster_threshold = NULL) {
  d_places <- df %>%
    filter(!is.na(place_grp)) %>%
    mutate(
      lag_rownum = lag(rw_num),
      rw_diff = rw_num - lag_rownum,
      clust_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
      cluster_grp = cumsum(clust_break)
    ) %>%
    select(-c(clust_break, lag_rownum, rw_diff, clust_break))

  clust_join <- suppressMessages(full_join(df, d_places))

  if (!is.null(cluster_threshold)) {
    if (!is.numeric(cluster_threshold)) {
      stop("Invalid 'type' of argument 'cluster_threshold.' Expecting a numeric value.", call. = FALSE)
    }

    dc <- clust_join %>%
      group_by(cluster_grp) %>%
      mutate(cluster_nrow = ifelse(is.na(cluster_grp), NA, n())) %>%
      ungroup()

    clust_n <- dc %>%
      group_by(cluster_nrow) %>%
      summarise(n_max = max(cluster_nrow)) %>%
      tidyr::drop_na() %>%
      .$n_max

    rm_clust <- sum(clust_n < cluster_threshold)
    
    d_clust <- dc %>% select(-c(move_break, rw_num, place_grp, place_lapse_grp, pl_distance, cluster_nrow))

    if (rm_clust > 0) {
      
      dc[!is.na(dc$cluster_nrow) & dc$cluster_nrow < cluster_threshold, "place_grp"] <- NA
      
      dc_rm <- dc[, !grepl("cluster_grp", colnames(dc))]

      reorder_clust <- dc_rm %>%
        filter(!is.na(place_grp)) %>%
        mutate(
          lag_rownum = lag(rw_num),
          rw_diff = rw_num - lag_rownum,
          clust_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
          cluster_grp = cumsum(clust_break)
        ) %>%
        select(-c(clust_break, lag_rownum, rw_diff, clust_break))

      d_clust <- suppressMessages(full_join(dc_rm, reorder_clust))
      d_clust <- d_clust %>%
        select(-c(move_break, rw_num, place_lapse_grp, place_grp, clustered_coord, pl_distance, cluster_nrow))

      message(paste(
        "A total of", rm_clust,
        "identified clusters had observations fewer than the 'cluster_threshold.'",
        "\n These observations were retained, but unclustered."
      ))
    }
  } else {
    d_clust <- clust_join %>%
      select(-c(move_break, rw_num, place_lapse_grp, place_grp, clustered_coord, pl_distance))
  }
  d_clust
}
