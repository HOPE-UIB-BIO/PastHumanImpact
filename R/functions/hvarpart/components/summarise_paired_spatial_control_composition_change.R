#' @title Summarise paired change in spatially controlled compositions
#' @description
#' Pair the region-age compositions of a reference and a comparison model and
#' summarise how the human-minus-climate difference changes between them,
#' for the zero-truncated allocation and the signed hierarchical
#' contributions.
#' @param data_composition
#' Composition table with `model_id`, `region`, `age`, `predictor`,
#' `individual`, and `allocation`, such as the `composition_common` element
#' of `prepare_human_proxy_comparison_tables()` bound across models.
#' @param reference_model_id
#' Identifier of the reference model.
#' @param comparison_model_id
#' Identifier of the comparison model.
#' @return
#' A list with `unit_pairs` (one row per paired region-age unit with
#' reference, comparison, and change values of `zero_difference` and
#' `signed_difference`, and ranking-change flags), `summary` (one row per
#' profile, `zero_truncated` and `signed`: `n_units`, the mean and median of
#' the reference, comparison, and change, the number of units with human
#' greater than climate in each model, and `n_ranking_changed`), and
#' `exclusions` (units present in only one model, with `present_in`).
#' @details
#' `zero_difference` is the human allocation minus the climate allocation;
#' `signed_difference` is the human minus the climate hierarchical
#' contribution. A unit enters a model only when both its human and climate
#' rows have finite values. Change is comparison minus reference.
#' @examples
#' \dontrun{
#' summarise_paired_spatial_control_composition_change(
#'   data_composition = compositions,
#'   reference_model_id = "spd__full_climate",
#'   comparison_model_id = "spd__filtered_climate"
#' )
#' }
summarise_paired_spatial_control_composition_change <- function(
  data_composition,
  reference_model_id,
  comparison_model_id
) {
  vec_unit_columns <-
    c("region", "age")

  assertthat::assert_that(
    is.data.frame(data_composition),
    all(c(
      "model_id", vec_unit_columns, "predictor", "individual", "allocation"
    ) %in% names(data_composition)),
    assertthat::is.string(reference_model_id),
    assertthat::is.string(comparison_model_id),
    reference_model_id != comparison_model_id,
    all(c(reference_model_id, comparison_model_id) %in%
      data_composition[["model_id"]]),
    msg = "Paired composition-change inputs do not satisfy the contract."
  )

  data_unit_differences <-
    data_composition |>
    dplyr::filter(
      .data[["model_id"]] %in% c(reference_model_id, comparison_model_id),
      .data[["predictor"]] %in% c("human", "climate")
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "model_id", vec_unit_columns, "predictor", "individual", "allocation"
      ))
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c("model_id", vec_unit_columns)),
      names_from = "predictor",
      values_from = c("individual", "allocation")
    ) |>
    dplyr::filter(
      is.finite(.data[["allocation_human"]]),
      is.finite(.data[["allocation_climate"]]),
      is.finite(.data[["individual_human"]]),
      is.finite(.data[["individual_climate"]])
    ) |>
    dplyr::transmute(
      model_role = dplyr::if_else(
        .data[["model_id"]] == reference_model_id,
        "reference",
        "comparison"
      ),
      dplyr::across(dplyr::all_of(vec_unit_columns)),
      zero_difference =
        .data[["allocation_human"]] - .data[["allocation_climate"]],
      signed_difference =
        .data[["individual_human"]] - .data[["individual_climate"]]
    )

  data_units_wide <-
    data_unit_differences |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(vec_unit_columns),
      names_from = "model_role",
      values_from = c("zero_difference", "signed_difference"),
      names_glue = "{model_role}_{.value}"
    )

  for (column_name in c(
    "reference_zero_difference", "comparison_zero_difference",
    "reference_signed_difference", "comparison_signed_difference"
  )) {
    if (
      !column_name %in% names(data_units_wide)
    ) {
      data_units_wide[[column_name]] <- rep(NA_real_, nrow(data_units_wide))
    }
  }

  data_exclusions <-
    data_units_wide |>
    dplyr::filter(
      is.na(.data[["reference_zero_difference"]]) |
        is.na(.data[["comparison_zero_difference"]])
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(vec_unit_columns)),
      present_in = dplyr::if_else(
        is.na(.data[["reference_zero_difference"]]),
        comparison_model_id,
        reference_model_id
      )
    )

  data_unit_pairs <-
    data_units_wide |>
    dplyr::filter(
      !is.na(.data[["reference_zero_difference"]]),
      !is.na(.data[["comparison_zero_difference"]])
    ) |>
    dplyr::mutate(
      change_zero_difference =
        .data[["comparison_zero_difference"]] -
          .data[["reference_zero_difference"]],
      change_signed_difference =
        .data[["comparison_signed_difference"]] -
          .data[["reference_signed_difference"]],
      zero_ranking_changed =
        sign(.data[["reference_zero_difference"]]) !=
          sign(.data[["comparison_zero_difference"]]),
      signed_ranking_changed =
        sign(.data[["reference_signed_difference"]]) !=
          sign(.data[["comparison_signed_difference"]]),
      reference_model_id = reference_model_id,
      comparison_model_id = comparison_model_id,
      .before = 1L
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(vec_unit_columns)))

  data_summary <-
    tibble::tibble(
      profile = c("zero_truncated", "signed"),
      value_column = c("zero_difference", "signed_difference"),
      flag_column = c("zero_ranking_changed", "signed_ranking_changed")
    ) |>
    purrr::pmap(
      .f = ~ {
        vec_reference <-
          data_unit_pairs[[stringr::str_c("reference_", ..2)]]

        vec_comparison <-
          data_unit_pairs[[stringr::str_c("comparison_", ..2)]]

        vec_change <-
          data_unit_pairs[[stringr::str_c("change_", ..2)]]

        tibble::tibble(
          profile = ..1,
          n_units = nrow(data_unit_pairs),
          reference_mean = mean(vec_reference),
          reference_median = stats::median(vec_reference),
          comparison_mean = mean(vec_comparison),
          comparison_median = stats::median(vec_comparison),
          change_mean = mean(vec_change),
          change_median = stats::median(vec_change),
          reference_n_human = sum(vec_reference > 0),
          comparison_n_human = sum(vec_comparison > 0),
          n_ranking_changed = sum(data_unit_pairs[[..3]])
        )
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      reference_model_id = reference_model_id,
      comparison_model_id = comparison_model_id,
      .before = 1L
    )

  res <-
    list(
      unit_pairs = data_unit_pairs,
      summary = data_summary,
      exclusions = data_exclusions
    )

  return(res)
}
