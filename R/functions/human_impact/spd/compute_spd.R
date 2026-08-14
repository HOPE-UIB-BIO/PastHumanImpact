#' @title Compute one summed probability distribution
#' @description Calibrate eligible radiocarbon dates and calculate one SPD.
#' @param data_source Radiocarbon-date table containing distance, age, error,
#'   and laboratory identifier columns.
#' @param sel_dist Maximum distance represented by the SPD.
#' @param sel_calcurve Calibration-curve table.
#' @param max_age Oldest age included in the SPD.
#' @param min_age Youngest age included in the SPD.
#' @param sel_smooth_size Running-mean smoothing window.
#' @param min_n_dates Minimum number of dates required.
#' @param normalise_to_one Whether to normalise total SPD mass to one.
#' @return An SPD grid tibble, or `NA` when too few dates are available.
compute_spd <- function(
  data_source,
  sel_dist,
  sel_calcurve,
  max_age,
  min_age,
  sel_smooth_size,
  min_n_dates,
  normalise_to_one
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c("dist", "Age", "Error", "LabID") %in% names(data_source)),
    is.data.frame(sel_calcurve),
    "C14BP" %in% names(sel_calcurve),
    is.numeric(sel_dist),
    length(sel_dist) == 1L,
    is.finite(sel_dist),
    is.numeric(max_age),
    length(max_age) == 1L,
    is.finite(max_age),
    is.numeric(min_age),
    length(min_age) == 1L,
    is.finite(min_age),
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

  data_sub <-
    data_source |>
    dplyr::filter(.data[["dist"]] < sel_dist) |>
    dplyr::filter(.data[["Age"]] <= max_age * 2) |>
    dplyr::filter(.data[["Age"]] > min(sel_calcurve[["C14BP"]]))

  if (
    nrow(data_sub) <= min_n_dates
  ) {
    return(NA)
  }

  data_calibrated <-
    rcarbon::calibrate(
      x = data_sub[["Age"]],
      errors = data_sub[["Error"]],
      ids = data_sub[["LabID"]],
      calCurves = sel_calcurve,
      verbose = TRUE,
      ncores = 1
    )

  data_spd <-
    rcarbon::spd(
      x = data_calibrated,
      timeRange = c(max_age, min_age),
      spdnormalised = normalise_to_one,
      runm = sel_smooth_size,
      verbose = TRUE
    )

  res_spd <-
    data_spd[["grid"]] |>
    tibble::as_tibble()

  return(res_spd)
}
