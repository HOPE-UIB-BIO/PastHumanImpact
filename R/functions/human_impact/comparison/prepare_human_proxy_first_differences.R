#' @title Prepare human-proxy changes toward the present
#' @description
#' Calculate consecutive within-dataset changes from an older observation to
#' the next younger observation for trend-robust sensitivity analysis.
#' @param data_matched Matched human-proxy observations.
#' @return Matched table with transformed proxy columns replaced by changes.
#' @examples
#' \dontrun{
#' prepare_human_proxy_first_differences(data_matched)
#' }
prepare_human_proxy_first_differences <- function(data_matched) {
  value_columns <-
    c("spd_transformed", "kk10_transformed", "hyde_transformed")

  assertthat::assert_that(
    is.data.frame(data_matched),
    all(c("dataset_id", "age_bp", value_columns) %in% names(data_matched)),
    msg = "Human-proxy first-difference inputs are invalid."
  )

  res_differences <-
    data_matched |>
    dplyr::group_by(.data[["dataset_id"]]) |>
    dplyr::arrange(dplyr::desc(.data[["age_bp"]]), .by_group = TRUE) |>
    dplyr::mutate(
      age_bp_older = dplyr::lag(.data[["age_bp"]]),
      dplyr::across(
        dplyr::all_of(value_columns),
        ~ .x - dplyr::lag(.x)
      )
    ) |>
    dplyr::filter(!is.na(.data[["age_bp_older"]])) |>
    dplyr::ungroup()

  return(res_differences)
}
