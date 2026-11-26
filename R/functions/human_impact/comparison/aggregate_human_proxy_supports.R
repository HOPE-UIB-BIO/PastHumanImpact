#' @title Aggregate a gridded human proxy over every record support
#' @description
#' Extract a gridded human proxy at each record's grid cell and as an
#' area-weighted mean within each buffer support, using
#' `aggregate_human_proxy_raster()` for the raster work.
#' @param raster_source
#' A longitude-latitude `SpatRaster`.
#' @param data_support_locations
#' Data frame from `prepare_human_proxy_support_locations()` with
#' `location_id`, `dataset_id`, `support`, `radius_km`, `extraction`, `long`,
#' and `lat`.
#' @param data_layers
#' Data frame with `layer_index` and `age_bp`, one row per extracted layer.
#' @param proxy
#' Character scalar proxy name written to the output.
#' @param aggregation
#' One of `area_weighted_mean` (default) or `cell_center_sum`, used for
#' buffer supports.
#' @return
#' Tibble with one row per location and layer and the columns `dataset_id`,
#' `support`, `radius_km`, `age_bp`, `proxy`, `value`, `n_cells`, and
#' `coverage_weight`. `value` is the value of the containing cell for
#' `cell` support and the buffer aggregate otherwise. `radius_km` is copied
#' from `data_support_locations`. `value` is `NA` where the raster has no
#' finite value at the support.
#' @examples
#' \dontrun{
#' aggregate_human_proxy_supports(raster, locations, layers, "kk10")
#' }
aggregate_human_proxy_supports <- function(
  raster_source,
  data_support_locations,
  data_layers,
  proxy,
  aggregation = "area_weighted_mean"
) {
  required_columns <-
    c(
      "location_id",
      "dataset_id",
      "support",
      "radius_km",
      "extraction",
      "long",
      "lat"
    )

  assertthat::assert_that(
    inherits(raster_source, "SpatRaster"),
    is.data.frame(data_support_locations),
    all(required_columns %in% names(data_support_locations)),
    nrow(data_support_locations) > 0L,
    !anyDuplicated(data_support_locations[["location_id"]]),
    all(
      data_support_locations[["extraction"]] %in% c("point", "buffer")
    ),
    is.data.frame(data_layers),
    assertthat::is.string(proxy),
    assertthat::is.string(aggregation),
    msg = "Human-proxy support aggregation inputs do not satisfy the contract."
  )

  data_point_locations <-
    data_support_locations |>
    dplyr::filter(.data[["extraction"]] == "point") |>
    # The point extraction ignores the radius, but the raster helper requires
    # a positive one for every location.
    dplyr::mutate(
      radius_km = 1
    ) |>
    dplyr::select(
      dataset_id = "location_id",
      dplyr::all_of(c("long", "lat", "radius_km"))
    )

  data_buffer_locations <-
    data_support_locations |>
    dplyr::filter(.data[["extraction"]] == "buffer") |>
    dplyr::select(
      dataset_id = "location_id",
      dplyr::all_of(c("long", "lat", "radius_km"))
    )

  list_extracted <-
    list(
      point = data_point_locations,
      buffer = data_buffer_locations
    ) |>
    purrr::keep(
      .p = ~ nrow(.x) > 0L
    ) |>
    purrr::imap(
      .f = ~ aggregate_human_proxy_raster(
        raster_source = raster_source,
        data_locations = .x,
        data_layers = data_layers,
        proxy = proxy,
        aggregation = aggregation,
        extraction = .y
      )
    )

  data_location_keys <-
    data_support_locations |>
    dplyr::select(
      dplyr::all_of(
        c("location_id", "dataset_id", "support", "radius_km")
      )
    )

  res_values <-
    list_extracted |>
    dplyr::bind_rows() |>
    dplyr::select(
      location_id = "dataset_id",
      dplyr::all_of(
        c("age_bp", "value", "n_cells", "coverage_weight")
      )
    ) |>
    dplyr::inner_join(
      data_location_keys,
      by = dplyr::join_by("location_id"),
      relationship = "many-to-one",
      unmatched = "error"
    ) |>
    dplyr::mutate(
      proxy = proxy,
      value = dplyr::if_else(
        is.finite(.data[["value"]]),
        .data[["value"]],
        NA_real_
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "dataset_id",
          "support",
          "radius_km",
          "age_bp",
          "proxy",
          "value",
          "n_cells",
          "coverage_weight"
        )
      )
    )

  return(res_values)
}
