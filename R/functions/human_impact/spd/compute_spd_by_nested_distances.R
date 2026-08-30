#' @title Compute nested-distance SPDs with one calibration
#' @description
#' Calibrate the largest-distance radiocarbon set once, then reuse the exact
#' calibrated-date subsets for nested smaller radii.
#' @param data_source_c14 One-row filtered radiocarbon dataset branch.
#' @param data_source_dist_vec Named, strictly increasing distance thresholds.
#' @param age_from Youngest modelled age.
#' @param age_to Oldest modelled age.
#' @param sel_smooth_size Running-mean smoothing window.
#' @param min_n_dates Minimum number of dates required.
#' @param normalise_to_one Whether to normalise total SPD mass to one.
#' @return One-row tibble with dataset identifier and nested radius columns.
#' @examples
#' \dontrun{
#' compute_spd_by_nested_distances(data_branch, c(`250` = 250, `500` = 500))
#' }
compute_spd_by_nested_distances <- function(
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
    nrow(data_source_c14) == 1L,
    all(c("curve_name", "rc", "dataset_id") %in% names(data_source_c14)),
    is.list(data_source_c14[["rc"]]),
    is.numeric(data_source_dist_vec),
    length(data_source_dist_vec) > 0L,
    all(is.finite(data_source_dist_vec)),
    !is.null(names(data_source_dist_vec)),
    all(nzchar(names(data_source_dist_vec))),
    identical(
      as.numeric(data_source_dist_vec),
      sort(unique(as.numeric(data_source_dist_vec)))
    ),
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
    msg = "Nested-distance SPD inputs do not satisfy the contract."
  )

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
      p = 0.5,
      resOffsets = 0,
      resErrors = 0
    ) |>
    tibble::as_tibble()
  calibration_curve <-
    switch(
      data_source_c14[["curve_name"]][[1]],
      "intcal20" = intcal20,
      "SHCal20" = shcal20,
      "mixed_curve20" = calmixed
    )

  if (is.null(calibration_curve)) {
    cli::cli_abort("The dataset calibration curve is not supported.")
  }

  data_rc <- data_source_c14[["rc"]][[1]]
  maximum_distance <- max(data_source_dist_vec)
  data_maximum <-
    data_rc |>
    dplyr::filter(
      .data[["dist"]] < maximum_distance,
      .data[["Age"]] <= age_to * 2,
      .data[["Age"]] > min(calibration_curve[["C14BP"]])
    )
  age_grid <- seq(from = age_to, to = age_from, by = -1)

  data_calibrated <-
    if (nrow(data_maximum) > min_n_dates) {
      rcarbon::calibrate(
        x = data_maximum[["Age"]],
        errors = data_maximum[["Error"]],
        ids = data_maximum[["LabID"]],
        calCurves = calibration_curve,
        verbose = FALSE,
        ncores = 1
      )
    } else {
      NULL
    }

  list_density <-
    purrr::map(
      data_source_dist_vec,
      ~ {
        index <- which(data_maximum[["dist"]] < .x)

        if (length(index) <= min_n_dates || is.null(data_calibrated)) {
          return(rep(0, length(age_grid)))
        }

        data_spd <-
          rcarbon::spd(
            x = data_calibrated[index],
            timeRange = c(age_to, age_from),
            spdnormalised = normalise_to_one,
            runm = sel_smooth_size,
            verbose = FALSE
          )

        return(data_spd[["grid"]][["PrDens"]])
      }
    ) |>
    rlang::set_names(names(data_source_dist_vec))
  data_spd <-
    dplyr::bind_cols(
      tibble::tibble(age = age_grid),
      tibble::as_tibble(list_density)
    )
  res <-
    tibble::tibble(
      dataset_id = data_source_c14[["dataset_id"]][[1]],
      spd = list(data_spd)
    )

  return(res)
}
