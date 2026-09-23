#' @title Aggregate gridded human proxies around pollen records
#' @description
#' Extract one or more raster layers at pollen-record points or within
#' record-specific geodesic buffers and return long-form proxy values.
#' @param raster_source A longitude-latitude `SpatRaster`.
#' @param data_locations Data frame with `dataset_id`, `long`, `lat`, and
#'   `radius_km`.
#' @param data_layers Data frame with `layer_index` and `age_bp`.
#' @param proxy Character scalar output proxy name.
#' @param aggregation One of `area_weighted_mean` or `cell_center_sum`.
#' @param extraction One of `buffer` or `point`.
#' @return Tibble with one row per dataset and requested age.
#' @examples
#' \dontrun{
#' aggregate_human_proxy_raster(raster, locations, layers, "kk10")
#' }
aggregate_human_proxy_raster <- function(
  raster_source,
  data_locations,
  data_layers,
  proxy,
  aggregation = "area_weighted_mean",
  extraction = "buffer"
) {
  assertthat::assert_that(
    inherits(raster_source, "SpatRaster"),
    terra::is.lonlat(raster_source),
    is.data.frame(data_locations),
    all(c("dataset_id", "long", "lat", "radius_km") %in%
      names(data_locations)),
    !anyDuplicated(data_locations[["dataset_id"]]),
    is.data.frame(data_layers),
    all(c("layer_index", "age_bp") %in% names(data_layers)),
    assertthat::is.string(proxy),
    aggregation %in% c(
      "area_weighted_mean",
      "cell_center_sum"
    ),
    extraction %in% c("buffer", "point"),
    msg = "Gridded human-proxy aggregation inputs are invalid."
  )

  assertthat::assert_that(
    all(is.finite(data_locations[["long"]])),
    all(is.finite(data_locations[["lat"]])),
    all(is.finite(data_locations[["radius_km"]])),
    all(data_locations[["radius_km"]] > 0),
    all(data_layers[["layer_index"]] %in%
      seq_len(terra::nlyr(raster_source))),
    !anyDuplicated(data_layers[["age_bp"]]),
    msg = "Human-proxy locations or layer selections are invalid."
  )

  raster_selected <-
    raster_source[[data_layers[["layer_index"]]]]

  names(raster_selected) <-
    stringr::str_c("age_", data_layers[["age_bp"]])

  points <-
    terra::vect(
      data_locations,
      geom = c("long", "lat"),
      crs = "EPSG:4326",
      keepgeom = FALSE
    )

  if (
    identical(extraction, "point")
  ) {
    data_extracted <-
      terra::extract(raster_selected, points) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        dataset_id = data_locations[["dataset_id"]][.data[["ID"]]],
        radius_km = data_locations[["radius_km"]][.data[["ID"]]],
        n_cells = 1L,
        coverage_weight = 1
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("age_"),
        names_to = "age_name",
        values_to = "value"
      ) |>
      dplyr::mutate(
        age_bp = as.numeric(stringr::str_remove(.data[["age_name"]], "age_")),
        proxy = proxy,
        extraction = extraction,
        aggregation = "point"
      ) |>
      dplyr::select(
        dplyr::all_of(
          c(
            "dataset_id",
            "age_bp",
            "proxy",
            "value",
            "radius_km",
            "extraction",
            "aggregation",
            "n_cells",
            "coverage_weight"
          )
        )
      )

    return(data_extracted)
  }

  # Keep cell-centre intermediates bounded: the full 250/500 km extraction
  # can otherwise require several GB before it is reduced to record summaries.
  data_location_tiles <-
    data_locations |>
    dplyr::transmute(
      row_id = dplyr::row_number(),
      tile_id = stringr::str_c(
        floor((.data[["lat"]] + 90) / 90),
        floor((.data[["long"]] + 180) / 90),
        sep = "_"
      )
    )

  location_tiles <-
    split(
      data_location_tiles[["row_id"]],
      data_location_tiles[["tile_id"]]
    )

  location_chunks <-
    location_tiles |>
    purrr::map(
      ~ split(
        as.integer(.x),
        ceiling(seq_along(.x) / 500L)
      )
    ) |>
    purrr::list_flatten()

  res_data <-
    location_chunks |>
    purrr::imap(
      ~ {
        cli::cli_inform(
          c(
            "i" = paste(
              "Extracting buffer chunk",
              .y,
              "of",
              length(location_chunks)
            )
          )
        )
        compute_human_proxy_buffer_chunk(
          raster_selected = raster_selected,
          data_locations = data_locations[.x, , drop = FALSE],
          proxy = proxy,
          aggregation = aggregation
        )
      }
    ) |>
    dplyr::bind_rows()
  return(res_data)
}
