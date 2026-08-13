#' @title Compute an SPD density vector
#' @description Calculate an SPD and return its probability-density values.
#' @inheritParams compute_spd
#' @return Numeric probability-density vector, or zero when unavailable.
compute_spd_density <- function(
  data_source,
  sel_dist,
  sel_calcurve,
  min_n_dates,
  max_age,
  min_age,
  sel_smooth_size,
  normalise_to_one
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.numeric(sel_dist),
    length(sel_dist) == 1L,
    is.data.frame(sel_calcurve),
    is.numeric(min_n_dates),
    length(min_n_dates) == 1L,
    is.numeric(max_age),
    length(max_age) == 1L,
    is.numeric(min_age),
    length(min_age) == 1L,
    is.numeric(sel_smooth_size),
    length(sel_smooth_size) == 1L,
    is.logical(normalise_to_one),
    length(normalise_to_one) == 1L,
    msg = "SPD density inputs do not satisfy the required contract."
  )

  data_spd <-
    compute_spd(
      data_source = data_source,
      sel_dist = sel_dist,
      sel_calcurve = sel_calcurve,
      min_n_dates = min_n_dates,
      max_age = max_age,
      min_age = min_age,
      sel_smooth_size = sel_smooth_size,
      normalise_to_one = normalise_to_one
    )

  if (
    all(is.na(data_spd))
  ) {
    return(0)
  }

  res_density <-
    data_spd[["PrDens"]]

  return(res_density)
}
