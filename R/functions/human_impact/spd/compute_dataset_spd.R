#' @title Compute all distance-based SPDs for one dataset
#' @description Apply the configured distance classes to one dataset.
#' @param rc Radiocarbon-date table for one dataset.
#' @param calcurve Calibration-curve table for the dataset.
#' @param dataset_id Dataset identifier used for progress output.
#' @param dummy_age_table Complete age grid used to align SPD values.
#' @param data_source_dist_vec Numeric distance thresholds.
#' @param min_n_dates Minimum number of radiocarbon dates.
#' @param age_to Oldest modelled age.
#' @param age_from Youngest modelled age.
#' @param sel_smooth_size Running-mean smoothing window.
#' @param normalise_to_one Whether to normalise total SPD mass to one.
#' @return A tibble containing age and one SPD column per distance threshold.
compute_dataset_spd <- function(
  rc,
  calcurve,
  dataset_id,
  dummy_age_table,
  data_source_dist_vec,
  min_n_dates,
  age_to,
  age_from,
  sel_smooth_size,
  normalise_to_one
) {
  assertthat::assert_that(
    is.data.frame(rc),
    is.data.frame(calcurve),
    length(dataset_id) == 1L,
    is.data.frame(dummy_age_table),
    "age" %in% names(dummy_age_table),
    is.numeric(data_source_dist_vec),
    length(data_source_dist_vec) > 0L,
    msg = "Dataset-level SPD inputs do not satisfy the required contract."
  )

  message(dataset_id)

  data_density <-
    data_source_dist_vec |>
    purrr::map(
      compute_spd_density,
      data_source = rc,
      sel_calcurve = calcurve,
      min_n_dates = min_n_dates,
      max_age = age_to,
      min_age = age_from,
      sel_smooth_size = sel_smooth_size,
      normalise_to_one = normalise_to_one
    ) |>
    dplyr::bind_cols()

  res_spd <-
    dplyr::bind_cols(dummy_age_table, data_density)

  return(res_spd)
}
