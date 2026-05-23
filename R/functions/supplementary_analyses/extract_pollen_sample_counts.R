#' @title Extract sample-level pollen counts with spatial and temporal bins
#' @description
#' Expands nested pollen level and harmonised count tables into a sample-level
#' table with grain-count bins, age bins, and climate-zone labels.
#' @param data_source
#' A data frame with columns `dataset_id`, `region`, `ecozone_koppen_5`,
#' `ecozone_koppen_15`, `ecozone_koppen_30`, `pollen_percentage`, `levels`, and
#' `counts_harmonised`.
#' @param stage_label
#' Character label describing the processing stage, for example `pre_filter`.
#' @param time_step
#' Numeric time-bin size in years. Default is `500`.
#' @return
#' A tibble with one row per sample containing `dataset_id`, `region`,
#' `climatezone`, `sample_id`, `age`, `rowsum`, `grain_bin`, and stage labels.
#' @examples
#' \dontrun{
#' extract_pollen_sample_counts(
#'   data_source = data_assembly,
#'   stage_label = "pre_filter"
#' )
#' }
extract_pollen_sample_counts <- function(data_source,
                                         stage_label,
                                         time_step = 500) {
  required_cols <-
    c(
      "dataset_id",
      "region",
      "ecozone_koppen_5",
      "ecozone_koppen_15",
      "ecozone_koppen_30",
      "pollen_percentage",
      "levels",
      "counts_harmonised"
    )

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    all(required_cols %in% names(data_source)),
    msg = "`data_source` is missing required columns for extraction."
  )

  assertthat::assert_that(
    is.character(stage_label) && length(stage_label) == 1,
    msg = "`stage_label` must be a single character value."
  )

  assertthat::assert_that(
    is.numeric(time_step) && length(time_step) == 1 && time_step > 0,
    msg = "`time_step` must be a single positive number."
  )

  res_table <-
    data_source |>
    dplyr::select(dplyr::all_of(required_cols)) |>
    dplyr::mutate(
      data_levels = purrr::map(
        .x = levels,
        .f = ~ .x |>
          dplyr::select(dplyr::all_of(c("sample_id", "age")))
      ),
      data_rowsums = purrr::map(
        .x = counts_harmonised,
        .f = ~ get_sample_rowsum_table(data_counts = .x)
      ),
      data_samples = purrr::map2(
        .x = data_levels,
        .y = data_rowsums,
        .f = ~ dplyr::inner_join(
          .x,
          .y,
          by = "sample_id"
        )
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "dataset_id",
          "region",
          "ecozone_koppen_5",
          "ecozone_koppen_15",
          "ecozone_koppen_30",
          "pollen_percentage",
          "data_samples"
        )
      )
    ) |>
    tidyr::unnest(cols = dplyr::all_of("data_samples")) |>
    dplyr::mutate(
      stage = stage_label,
      climatezone = dplyr::case_when(
        ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
        ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
        ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
        .default = ecozone_koppen_5
      ),
      age_bin_start = floor(age / time_step) * time_step,
      age_bin_end = age_bin_start + time_step,
      age_bin = stringr::str_glue("{age_bin_start}-{age_bin_end}"),
      grain_bin = dplyr::case_when(
        rowsum < 25 ~ "<25",
        rowsum >= 25 & rowsum < 150 ~ "25-149",
        rowsum >= 150 & rowsum < 300 ~ "150-299",
        rowsum >= 300 & rowsum < 500 ~ "300-499",
        rowsum >= 500 ~ ">=500",
        TRUE ~ NA_character_
      )
    ) |>
    tibble::as_tibble()

  return(res_table)
}
