subset_c14_data <- function(data_source_c14,
                            data_source_polygons,
                            data_source_meta) {
  # make uniqu and select columns
  data_rc_work <-
    data_source_c14 %>%
    dplyr::distinct(LabID, .keep_all = TRUE) %>%
    dplyr::select(
      LabID,
      lat, long,
      Age, Error
    )

  # Convert to spatial object
  data_rc_poly <-
    data_rc_work %>%
    sf::st_as_sf(
      coords = c("long", "lat"),
      crs = 4326
    )

  # get overlap of RC and site polygons
  data_merge <-
    sf::st_join(
      data_rc_poly,
      data_source_polygons,
      left = FALSE
    ) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::select(
      LabID,
      dataset_id
    ) %>%
    dplyr::left_join(
      data_rc_work,
      by = "LabID"
    )

  # nest data for each dataset_id
  nest_by_sequence <-
    data_merge %>%
    dplyr::select(
      dataset_id,
      LabID, long, lat,
      Age, Error
    ) %>%
    dplyr::group_by(dataset_id) %>%
    tidyr::nest(
      rc = -dataset_id
    ) %>%
    dplyr::ungroup()

  # add long, lat back (and add curve name)
  data_for_distance <-
    data_source_meta %>%
    dplyr::select(dataset_id, long, lat, curve_name) %>%
    dplyr::left_join(
      nest_by_sequence,
      .,
      by = "dataset_id"
    )

  # estimate distance
  data_with_distance <-
    data_for_distance %>%
    dplyr::mutate(
      rc = purrr::pmap(
        .l = list(long, lat, rc),
        .f = ~ ..3 %>%
          dplyr::mutate(
            dist = geosphere::distGeo(
              p1 = c(..1, ..2),
              p2 = ..3 %>%
                dplyr::select(long, lat)
            ) / 1e3 # to convert to km
          )
      )
    )  %>% 
    dplyr::select(-c(long, lat))

    return(data_with_distance)
}
