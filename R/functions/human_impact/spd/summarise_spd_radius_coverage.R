#' @title Summarise SPD radius coverage
#' @description
#' Count available radius-specific SPD products overall and across the
#' repository's geographic reporting groups.
#' @param data_spd Validated radius-specific SPD products.
#' @param data_meta Metadata containing dataset, region, and climate zone.
#' @return Long coverage table with summary level, geography, radius, counts,
#'   and availability proportion.
#' @examples
#' \dontrun{
#' coverage <- summarise_spd_radius_coverage(products, metadata)
#' }
summarise_spd_radius_coverage <- function(data_spd, data_meta) {
  assertthat::assert_that(
    is.data.frame(data_spd),
    all(
      c(
        "dataset_id",
        "radius_km",
        "n_time_points",
        "n_finite_values",
        "available"
      ) %in% names(data_spd)
    ),
    is.data.frame(data_meta),
    all(c("dataset_id", "region", "climatezone") %in% names(data_meta)),
    !anyDuplicated(data_meta[["dataset_id"]]),
    msg = "SPD radius coverage inputs do not satisfy the required contract."
  )

  data_geography <-
    data_meta |>
    dplyr::distinct(
      .data[["dataset_id"]],
      .data[["region"]],
      .data[["climatezone"]]
    )

  data_work <-
    data_spd |>
    dplyr::left_join(
      data_geography,
      by = "dataset_id",
      relationship = "many-to-one"
    )

  table_overall <-
    data_work |>
    dplyr::group_by(.data[["radius_km"]]) |>
    dplyr::summarise(
      n_datasets = dplyr::n(),
      n_series_available = sum(
        .data[["n_time_points"]] > 0 &
          .data[["n_finite_values"]] > 0
      ),
      n_valid_signal = sum(.data[["available"]]),
      n_available = .data[["n_valid_signal"]],
      proportion_valid_signal = mean(.data[["available"]]),
      proportion_available = .data[["proportion_valid_signal"]],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      summary_level = "overall",
      region = NA_character_,
      climatezone = NA_character_,
      .before = 1L
    )

  table_region <-
    data_work |>
    dplyr::group_by(.data[["radius_km"]], .data[["region"]]) |>
    dplyr::summarise(
      n_datasets = dplyr::n(),
      n_series_available = sum(
        .data[["n_time_points"]] > 0 &
          .data[["n_finite_values"]] > 0
      ),
      n_valid_signal = sum(.data[["available"]]),
      n_available = .data[["n_valid_signal"]],
      proportion_valid_signal = mean(.data[["available"]]),
      proportion_available = .data[["proportion_valid_signal"]],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      summary_level = "region",
      climatezone = NA_character_,
      .before = 1L
    )

  table_climatezone <-
    data_work |>
    dplyr::group_by(
      .data[["radius_km"]],
      .data[["climatezone"]]
    ) |>
    dplyr::summarise(
      n_datasets = dplyr::n(),
      n_series_available = sum(
        .data[["n_time_points"]] > 0 &
          .data[["n_finite_values"]] > 0
      ),
      n_valid_signal = sum(.data[["available"]]),
      n_available = .data[["n_valid_signal"]],
      proportion_valid_signal = mean(.data[["available"]]),
      proportion_available = .data[["proportion_valid_signal"]],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      summary_level = "climatezone",
      region = NA_character_,
      .before = 1L
    )

  table_region_climatezone <-
    data_work |>
    dplyr::group_by(
      .data[["radius_km"]],
      .data[["region"]],
      .data[["climatezone"]]
    ) |>
    dplyr::summarise(
      n_datasets = dplyr::n(),
      n_series_available = sum(
        .data[["n_time_points"]] > 0 &
          .data[["n_finite_values"]] > 0
      ),
      n_valid_signal = sum(.data[["available"]]),
      n_available = .data[["n_valid_signal"]],
      proportion_valid_signal = mean(.data[["available"]]),
      proportion_available = .data[["proportion_valid_signal"]],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      summary_level = "region_and_climatezone",
      .before = 1L
    )

  res_coverage <-
    dplyr::bind_rows(
      table_overall,
      table_region,
      table_climatezone,
      table_region_climatezone
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "summary_level",
        "region",
        "climatezone",
        "radius_km",
        "n_datasets",
        "n_series_available",
        "n_valid_signal",
        "n_available",
        "proportion_valid_signal",
        "proportion_available"
      ))
    )

  return(res_coverage)
}
