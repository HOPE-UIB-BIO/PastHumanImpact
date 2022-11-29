make_polygons <- function(data_source,
                          n_points = 10,
                          distance_buffer = 10) {
  # helper functions
  create_dummy_points <- function(data_source_long,
                                  data_source_lat,
                                  n_points,
                                  distance) {
    point_tibble <-
      tibble::tibble(
        index = 1:n_points,
        long = NA,
        lat = NA,
        .rows = n_points
      )

    point_angle <- 360 / (n_points)

    angle_work <- (pi * point_angle / 180)

    res <-
      point_tibble %>%
      dplyr::mutate(
        long = data_source_long + distance * cos(angle_work * index),
        lat = data_source_lat + distance * sin(angle_work * index)
      ) %>%
      dplyr::select(-index)

    return(res)
  }

  hull_data <- function(data_source,
                        group_vec = c("continent", "dataset_id"),
                        include_both = TRUE) {
    data_work <-
      data_source %>%
      dplyr::mutate(data_type = "orginal")

    hull <-
      data_source %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vec)
        )
      ) %>%
      dplyr::slice(chull(lat, long)) %>%
      dplyr::mutate(data_type = "hull") %>%
      dplyr::ungroup()

    if (include_both == TRUE) {
      res <-
        bind_rows(
          data_work %>%
            dplyr::filter(!dataset_id %in% hull$dataset_id),
          hull
        )
    } else {
      res <- hull
    }
    return(res)
  }

  # make dummy points
  data_dummy <-
    data_source %>%
    dplyr::select(dataset_id, long, lat) %>%
    dplyr::mutate(
      dummy_points = purrr::map2(
        .x = long,
        .y = lat,
        .f = ~ create_dummy_points(
          data_source_long = .x,
          data_source_lat = .y,
          distance = distance_buffer,
          n_points = n_points
        )
      )
    ) %>%
    dplyr::select(-c(lat, long)) %>%
    tidyr::unnest(dummy_points)


  # Create polygons -----

  # chull the points
  polygon_tibble <-
    hull_data(
      data_source = data_dummy,
      group_vec = "dataset_id",
      include_both = FALSE
    ) %>%
    dplyr::filter(data_type == "hull") %>%
    dplyr::select(dataset_id, lat, long)


  # make into lists
  polygon_list <-
    split(polygon_tibble, polygon_tibble$dataset_id)

  # turn into class of Polygon
  polygon_list_class <-
    purrr::map(
      .x = polygon_list,
      .f = ~ .x %>%
        dplyr::select(long, lat) %>%
        sp::Polygon()
    )

  # turn into class Polygons
  polygons <-
    purrr::imap(
      .x = polygon_list_class,
      .f = ~ sp::Polygons(list(.x), ID = .y)
    )

  # turn into class SpatialPolygons
  spatial_polygons <-
    sp::SpatialPolygons(
      polygons,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    )

  # table to add dataset_id
  tranlation_table_sequence <-
    polygon_tibble %>%
    dplyr::distinct(dataset_id) %>%
    dplyr::mutate(id = dataset_id) %>%
    as.data.frame()

  # add dataset_id to polygons
  spatial_polygons_df <-
    sp::SpatialPolygonsDataFrame(
      spatial_polygons,
      tranlation_table_sequence,
      match.ID = "id"
    )

  return(spatial_polygons_df)
}
