#' @title Compute SPD for each distance class
#' @description For each pollen record, one SPD was calculated for each distance
#' class, including all data in the that distance.
#' Radiocarbon dates were calibrated  with apropriate calibration curves and
#' assigned by their geographical location following Hua et al., 2013.
#' SPD was only estimated  for each distance class but only if a distance
#' class have at least `min_n_dates` RC dates to maintain only robust SPD
#' estimation. For each SPD a total probability mass of the SPD is normalised
#' to sum to unity
compute_spd_by_distance <- function(
  data_source_c14,
  data_source_dist_vec,
  age_from = 0,
  age_to = 12e3,
  sel_smooth_size = 100,
  min_n_dates = 50,
  normalise_to_one = FALSE
) {
  assertthat::assert_that(
    is.data.frame(data_source_c14),
    all(c("curve_name", "rc", "dataset_id") %in% names(data_source_c14)),
    is.numeric(data_source_dist_vec),
    length(data_source_dist_vec) > 0L,
    all(is.finite(data_source_dist_vec)),
    is.numeric(age_from),
    length(age_from) == 1L,
    is.finite(age_from),
    is.numeric(age_to),
    length(age_to) == 1L,
    is.finite(age_to),
    age_to > age_from,
    is.numeric(sel_smooth_size),
    length(sel_smooth_size) == 1L,
    is.finite(sel_smooth_size),
    is.numeric(min_n_dates),
    length(min_n_dates) == 1L,
    is.finite(min_n_dates),
    is.logical(normalise_to_one),
    length(normalise_to_one) == 1L,
    !is.na(normalise_to_one),
    msg = "SPD inputs do not satisfy the required contract."
  )

  # Prepare calibration curves
  intcal20 <-
    rcarbon::mixCurves(
      calCurve1 = "intcal20",
      calCurve2 = "shcal20",
      p = 1,
      resOffsets = 0,
      resErrors = 0
    ) |>
    tibble::as_tibble()

  shcal20 <-
    rcarbon::mixCurves(
      calCurve1 = "shcal20",
      calCurve2 = "intcal20",
      p = 1,
      resOffsets = 0,
      resErrors = 0
    ) |>
    tibble::as_tibble()

  calmixed <-
    rcarbon::mixCurves(
      calCurve1 = "intcal20",
      calCurve2 = "shcal20",
      p = 0.5, # using the default p of 0.5
      resOffsets = 0,
      resErrors = 0
    ) |>
    tibble::as_tibble()

  data_rc_calcurve <-
    data_source_c14 |>
    dplyr::mutate(
      calcurve = purrr::map(
        .x = curve_name,
        .f = ~ switch(.x,
          "intcal20" = intcal20,
          "SHCal20" = shcal20,
          "mixed_curve20" = calmixed
        )
      )
    )

  # dummy table to bind all the results
  data_age <-
    tibble::tibble(
      age = seq(
        from = age_to,
        to = age_from,
        by = -1
      )
    )

  data_rc_calcurve <-
    data_rc_calcurve |>
    dplyr::mutate(
      dummy_age_table = list(data_age)
    )

  # get spd
  data_spd <-
    data_rc_calcurve |>
    dplyr::mutate(
      spd = purrr::pmap(
        .l = list(
          rc = .data[["rc"]],
          calcurve = .data[["calcurve"]],
          dataset_id = .data[["dataset_id"]],
          dummy_age_table = .data[["dummy_age_table"]]
        ),
        .f = compute_dataset_spd,
        data_source_dist_vec = data_source_dist_vec,
        min_n_dates = min_n_dates,
        age_to = age_to,
        age_from = age_from,
        sel_smooth_size = sel_smooth_size,
        normalise_to_one = normalise_to_one
      )
    )

  res_spd <-
    data_spd |>
    dplyr::select(
      .data[["dataset_id"]],
      .data[["spd"]]
    )

  return(res_spd)
}
