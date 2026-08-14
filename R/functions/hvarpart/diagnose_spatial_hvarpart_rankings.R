#' @title Diagnose human_climate_only and spatially controlled HVarPart rankings
#' @description
#' Compare human-minus-climate hierarchical contributions for human_climate_only and
#' spatially controlled region-age models. Explicit no-signal results retain
#' the human_climate_only ranking, whereas non-estimable spatial models remain missing.
#' @param data_components Extracted HVarPart component table.
#' @param data_status Extracted spatial model status table.
#' @return One row per analysis, region, and age with ranking diagnostics.
#' @examples
#' \dontrun{
#' diagnose_spatial_hvarpart_rankings(components, status)
#' }
diagnose_spatial_hvarpart_rankings <- function(
  data_components,
  data_status
) {
  keys <- c("analysis", "region", "age")
  required_components <-
    c(keys, "model_profile", "predictor", "Individual")
  required_status <- c(keys, "status", "n_selected")
  assertthat::assert_that(
    is.data.frame(data_components),
    is.data.frame(data_status),
    all(required_components %in% names(data_components)),
    all(required_status %in% names(data_status)),
    msg = "Spatial ranking inputs do not satisfy the contract."
  )

  data_wide <-
    data_components |>
    dplyr::filter(.data[["predictor"]] %in% c("human", "climate")) |>
    dplyr::select(dplyr::all_of(required_components)) |>
    tidyr::pivot_wider(
      names_from = c("model_profile", "predictor"),
      values_from = "Individual"
    )
  expected_value_columns <-
    c(
      "human_climate_human",
      "human_climate_climate",
      "human_climate_space_human",
      "human_climate_space_climate"
    )
  missing_value_columns <-
    setdiff(expected_value_columns, names(data_wide))
  data_wide <-
    dplyr::bind_cols(
      data_wide,
      rlang::set_names(
        rep(list(NA_real_), length(missing_value_columns)),
        missing_value_columns
      )
    )
  data_rankings <-
    data_status |>
    dplyr::select(dplyr::all_of(required_status)) |>
    dplyr::left_join(data_wide, by = keys) |>
    dplyr::mutate(
      human_climate_only_balance =
        .data[["human_climate_human"]] -
        .data[["human_climate_climate"]],
      controlled_human = dplyr::case_when(
        .data[["status"]] == "no_spatial_terms_selected" ~
          .data[["human_climate_human"]],
        .data[["status"]] == "spatial_model_estimated" ~
          .data[["human_climate_space_human"]],
        .default = NA_real_
      ),
      controlled_climate = dplyr::case_when(
        .data[["status"]] == "no_spatial_terms_selected" ~
          .data[["human_climate_climate"]],
        .data[["status"]] == "spatial_model_estimated" ~
          .data[["human_climate_space_climate"]],
        .default = NA_real_
      ),
      controlled_balance =
        .data[["controlled_human"]] -
        .data[["controlled_climate"]],
      human_climate_only_ranking = dplyr::case_when(
        .data[["human_climate_only_balance"]] > 0 ~ "human",
        .data[["human_climate_only_balance"]] < 0 ~ "climate",
        is.finite(.data[["human_climate_only_balance"]]) ~ "tie",
        .default = NA_character_
      ),
      controlled_ranking = dplyr::case_when(
        .data[["controlled_balance"]] > 0 ~ "human",
        .data[["controlled_balance"]] < 0 ~ "climate",
        is.finite(.data[["controlled_balance"]]) ~ "tie",
        .default = NA_character_
      ),
      ranking_changed = dplyr::case_when(
        is.na(.data[["human_climate_only_ranking"]]) |
          is.na(.data[["controlled_ranking"]]) ~ NA,
        .default = .data[["human_climate_only_ranking"]] !=
          .data[["controlled_ranking"]]
      )
    )

  return(data_rankings)
}
