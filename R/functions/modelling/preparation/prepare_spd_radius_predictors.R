#' @title Prepare predictor profiles for strict SPD radii
#' @description
#' Replace only the nested SPD column in canonical prepared predictor data with
#' interpolated strict-radius series.
#' @param data_predictors Canonical prepared predictors with `dataset_id` and
#'   nested `data_merge` tables.
#' @param data_spd_by_radius Matched radius products with nested `spd` tables.
#' @param age_min Minimum interpolation age.
#' @param age_max Maximum interpolation age.
#' @param timestep Interpolation timestep.
#' @return Predictor table keyed by radius specification and dataset.
#' @examples
#' \dontrun{
#' prepare_spd_radius_predictors(predictors, spd_by_radius)
#' }
prepare_spd_radius_predictors <- function(
  data_predictors,
  data_spd_by_radius,
  age_min = 0,
  age_max = 12e3,
  timestep = 500
) {
  assertthat::assert_that(
    is.data.frame(data_predictors),
    all(c("dataset_id", "data_merge") %in% names(data_predictors)),
    is.list(data_predictors[["data_merge"]]),
    !anyDuplicated(data_predictors[["dataset_id"]]),
    is.data.frame(data_spd_by_radius),
    all(
      c("dataset_id", "radius_km", "spd", "available") %in%
        names(data_spd_by_radius)
    ),
    is.list(data_spd_by_radius[["spd"]]),
    msg = "SPD radius predictor inputs do not satisfy the contract."
  )

  data_radius_counts <-
    data_spd_by_radius |>
    dplyr::count(.data[["dataset_id"]], name = "n_radii")

  if (
    !setequal(
      unique(data_spd_by_radius[["radius_km"]]),
      c(250L, 500L)
    ) ||
      any(data_radius_counts[["n_radii"]] != 2L)
  ) {
    cli::cli_abort(
      "Every prepared predictor dataset must contain every SPD radius."
    )
  }

  vec_output_ages <-
    seq(from = age_min, to = age_max, by = timestep)

  data_spd_interpolated <-
    data_spd_by_radius |>
    dplyr::mutate(
      spd_radius = purrr::map(
        .data[["spd"]],
        ~ tibble::tibble(
          age = vec_output_ages,
          spd = round(
            stats::approx(
              x = .x[["age"]],
              y = .x[["value"]],
              xout = vec_output_ages,
              method = "linear",
              rule = 1,
              ties = mean
            )[["y"]],
            digits = 3
          )
        )
      ),
      spd_radius_specification = stringr::str_c(
        .data[["radius_km"]],
        "_km"
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "dataset_id",
        "radius_km",
        "spd_radius_specification",
        "available",
        "spd_radius"
      ))
    )

  data_joined <-
    data_spd_interpolated |>
    dplyr::inner_join(
      data_predictors,
      by = "dataset_id",
      relationship = "many-to-one"
    )

  expected_rows <-
    nrow(data_predictors) *
      dplyr::n_distinct(data_spd_by_radius[["radius_km"]])

  if (nrow(data_joined) != expected_rows) {
    cli::cli_abort(
      "Every prepared predictor dataset must contain every SPD radius."
    )
  }

  res_predictors <-
    data_joined |>
    dplyr::mutate(
      data_merge = purrr::map2(
        .data[["data_merge"]],
        .data[["spd_radius"]],
        ~ {
          index <- match(.x[["age"]], .y[["age"]])

          if (anyNA(index)) {
            cli::cli_abort(
              "An SPD radius series does not cover the predictor age grid."
            )
          }

          .x[["spd"]] <- .y[["spd"]][index]
          .x
        }
      )
    ) |>
    dplyr::select(-dplyr::all_of("spd_radius")) |>
    dplyr::arrange(
      .data[["radius_km"]],
      .data[["dataset_id"]]
    )

  return(res_predictors)
}
