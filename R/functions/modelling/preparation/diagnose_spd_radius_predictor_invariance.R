#' @title Audit non-SPD predictor invariance across radius profiles
#' @description
#' Hash every nested predictor table after removing SPD and verify that only SPD
#' changes among radius profiles for the same dataset.
#' @param data_predictors_radius Radius-keyed predictor profiles.
#' @return Dataset-level audit table.
#' @examples
#' \dontrun{
#' diagnose_spd_radius_predictor_invariance(radius_predictors)
#' }
diagnose_spd_radius_predictor_invariance <- function(
  data_predictors_radius
) {
  assertthat::assert_that(
    is.data.frame(data_predictors_radius),
    all(
      c(
        "dataset_id",
        "radius_km",
        "data_merge"
      ) %in% names(data_predictors_radius)
    ),
    is.list(data_predictors_radius[["data_merge"]]),
    msg = "Radius predictor audit inputs do not satisfy the contract."
  )

  res_audit <-
    data_predictors_radius |>
    dplyr::mutate(
      non_spd_hash = purrr::map_chr(
        .data[["data_merge"]],
        ~ rlang::hash(
          dplyr::select(.x, -dplyr::any_of("spd"))
        )
      )
    ) |>
    dplyr::group_by(.data[["dataset_id"]]) |>
    dplyr::summarise(
      n_radius_profiles = dplyr::n(),
      n_non_spd_hashes = dplyr::n_distinct(.data[["non_spd_hash"]]),
      non_spd_identical = .data[["n_non_spd_hashes"]] == 1L,
      .groups = "drop"
    )

  return(res_audit)
}
