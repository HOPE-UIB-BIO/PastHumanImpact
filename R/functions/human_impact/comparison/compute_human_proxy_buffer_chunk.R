#' @title Extract one chunk of buffered human-proxy values
#' @description
#' Extract raster cells whose centres fall within a bounded group of record
#' buffers and immediately reduce the large cell table to record-age summaries.
#' @param raster_selected A longitude-latitude `SpatRaster` whose names encode
#' ages as `age_<years BP>`.
#' @param data_locations Data frame with `dataset_id`, `long`, `lat`, and
#' `radius_km`.
#' @param proxy Character scalar proxy name.
#' @param aggregation One of `area_weighted_mean` or `cell_center_sum`.
#' @return Tibble with one row per record and raster age.
compute_human_proxy_buffer_chunk <- function(
  raster_selected,
  data_locations,
  proxy,
  aggregation
) {
  points <-
    terra::vect(
      data_locations,
      geom = c("long", "lat"),
      crs = "EPSG:4326",
      keepgeom = FALSE
    )

  buffers <-
    terra::buffer(
      points,
      width = data_locations[["radius_km"]] * 1000
    )

  data_extracted <-
    terra::extract(
      raster_selected,
      buffers,
      cells = TRUE
    ) |>
    tibble::as_tibble()

  data_extracted[["overlap_weight"]] <- 1

  cell_latitudes <-
    terra::xyFromCell(
      raster_selected,
      data_extracted[["cell"]]
    )[, 2]

  half_cell_height <- terra::res(raster_selected)[[2]] / 2

  data_extracted[["cell_area_weight"]] <-
    abs(
      sin((cell_latitudes + half_cell_height) * pi / 180) -
        sin((cell_latitudes - half_cell_height) * pi / 180)
    )

  res_data <-
    data_extracted |>
    dplyr::mutate(
      dataset_id = data_locations[["dataset_id"]][.data[["ID"]]],
      radius_km = data_locations[["radius_km"]][.data[["ID"]]],
      area_overlap_weight =
        .data[["overlap_weight"]] * .data[["cell_area_weight"]]
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("age_"),
      names_to = "age_name",
      values_to = "cell_value"
    ) |>
    dplyr::mutate(
      age_bp = as.numeric(stringr::str_remove(.data[["age_name"]], "age_"))
    ) |>
    dplyr::group_by(
      .data[["dataset_id"]],
      .data[["radius_km"]],
      .data[["age_bp"]]
    ) |>
    dplyr::summarise(
      value = dplyr::case_when(
        all(!is.finite(.data[["cell_value"]])) ~ NA_real_,
        aggregation == "area_weighted_mean" ~ stats::weighted.mean(
          .data[["cell_value"]],
          .data[["area_overlap_weight"]],
          na.rm = TRUE
        ),
        .default = sum(
          .data[["cell_value"]] * .data[["overlap_weight"]],
          na.rm = TRUE
        )
      ),
      n_cells = dplyr::n_distinct(.data[["cell"]]),
      coverage_weight = sum(
        .data[["overlap_weight"]][is.finite(.data[["cell_value"]])],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      proxy = proxy,
      extraction = "buffer",
      aggregation = aggregation
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

  return(res_data)
}
