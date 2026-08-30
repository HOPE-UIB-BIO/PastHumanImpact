#' @title Compare an SPD analysis product with a reference artifact
#' @description
#' Compare nested age-value SPD series and selected distances without changing
#' either analysis product.
#' @param data_spd SPD product with `dataset_id`, nested `spd`, and `distance`.
#' @param data_reference Reference product with the same columns.
#' @param tolerance Maximum accepted absolute numeric difference.
#' @return Dataset-level comparison table describing cohort overlap, age-grid
#' identity, selected-distance identity, and numerical agreement.
#' @examples
#' \dontrun{
#' audit <- diagnose_spd_product_reference(new_fallback, old_fallback)
#' }
diagnose_spd_product_reference <- function(
  data_spd,
  data_reference,
  tolerance = sqrt(.Machine$double.eps)
) {
  assertthat::assert_that(
    is.data.frame(data_spd),
    all(c("dataset_id", "spd", "distance") %in% names(data_spd)),
    is.list(data_spd[["spd"]]),
    all(purrr::map_lgl(data_spd[["spd"]], is.data.frame)),
    all(purrr::map_lgl(
      data_spd[["spd"]],
      ~ all(c("age", "value") %in% names(.x))
    )),
    !anyDuplicated(data_spd[["dataset_id"]]),
    is.data.frame(data_reference),
    all(c("dataset_id", "spd", "distance") %in% names(data_reference)),
    is.list(data_reference[["spd"]]),
    all(purrr::map_lgl(data_reference[["spd"]], is.data.frame)),
    all(purrr::map_lgl(
      data_reference[["spd"]],
      ~ all(c("age", "value") %in% names(.x))
    )),
    !anyDuplicated(data_reference[["dataset_id"]]),
    is.numeric(tolerance),
    length(tolerance) == 1L,
    is.finite(tolerance),
    tolerance >= 0,
    msg = "SPD product comparison inputs do not satisfy the contract."
  )

  data_new <-
    data_spd |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd_new = .data[["spd"]],
      distance_new = .data[["distance"]]
    )

  data_old <-
    data_reference |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd_reference = .data[["spd"]],
      distance_reference = .data[["distance"]]
    )

  data_paired <-
    dplyr::full_join(
      data_new,
      data_old,
      by = "dataset_id"
    )

  res_comparison <-
    data_paired |>
    dplyr::mutate(
      present_new = !purrr::map_lgl(.data[["spd_new"]], is.null),
      present_reference =
        !purrr::map_lgl(.data[["spd_reference"]], is.null),
      identical_age_grid = purrr::map2_lgl(
        .data[["spd_new"]],
        .data[["spd_reference"]],
        ~ !is.null(.x) &&
          !is.null(.y) &&
          identical(.x[["age"]], .y[["age"]])
      ),
      identical_distance =
        .data[["distance_new"]] == .data[["distance_reference"]],
      max_absolute_difference = purrr::map2_dbl(
        .data[["spd_new"]],
        .data[["spd_reference"]],
        ~ {
          if (
            is.null(.x) || is.null(.y) ||
              !identical(.x[["age"]], .y[["age"]])
          ) {
            return(NA_real_)
          }

          max(abs(.x[["value"]] - .y[["value"]]), na.rm = TRUE)
        }
      ),
      within_tolerance =
        .data[["present_new"]] &
        .data[["present_reference"]] &
        .data[["identical_age_grid"]] &
        .data[["identical_distance"]] &
        is.finite(.data[["max_absolute_difference"]]) &
        .data[["max_absolute_difference"]] <= tolerance
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "dataset_id",
        "present_new",
        "present_reference",
        "distance_new",
        "distance_reference",
        "identical_distance",
        "identical_age_grid",
        "max_absolute_difference",
        "within_tolerance"
      ))
    )

  return(res_comparison)
}
