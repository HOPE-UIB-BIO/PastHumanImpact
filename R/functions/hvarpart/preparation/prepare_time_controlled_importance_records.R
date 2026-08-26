#' @title Prepare time-controlled dataset-level importance records
#' @description
#' Convert three-group within-dataset HVarPart components into signed
#' differences, conditional human-climate balances, and three-way presentation
#' allocations.
#' @param data_components Extracted temporal HVarPart component table.
#' @param data_status Extracted temporal HVarPart status table.
#' @param data_meta Core metadata with coordinates and spatial strata.
#' @param id_col Core identifier column.
#' @return One row per estimable time-controlled dataset model.
#' @examples
#' \dontrun{
#' prepare_time_controlled_importance_records(
#'   data_components = components,
#'   data_status = status,
#'   data_meta = metadata
#' )
#' }
prepare_time_controlled_importance_records <- function(
  data_components,
  data_status,
  data_meta,
  id_col = "dataset_id"
) {
  required_components <-
    c(
      id_col,
      "analysis",
      "predictor",
      "individual",
      "model_profile",
      "total_adjusted_r_squared"
    )
  required_status <- c(id_col, "analysis", "status")
  required_meta <- c(id_col, "long", "lat", "region", "climatezone")
  assertthat::assert_that(
    is.data.frame(data_components),
    all(required_components %in% names(data_components)),
    is.data.frame(data_status),
    all(required_status %in% names(data_status)),
    is.data.frame(data_meta),
    all(required_meta %in% names(data_meta)),
    msg = "Time-controlled importance inputs do not satisfy the contract."
  )

  data_wide <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate_time",
      .data[["predictor"]] %in% c("human", "climate", "time")
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        id_col,
        "analysis",
        "predictor",
        "individual",
        "total_adjusted_r_squared"
      ))
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(
        id_col,
        "analysis",
        "total_adjusted_r_squared"
      )),
      names_from = "predictor",
      values_from = "individual"
    )
  data_human_climate_only <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate",
      .data[["predictor"]] %in% c("human", "climate")
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        id_col,
        "analysis",
        "predictor",
        "individual",
        "total_adjusted_r_squared"
      ))
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(
        id_col,
        "analysis",
        "total_adjusted_r_squared"
      )),
      names_from = "predictor",
      values_from = "individual",
      names_prefix = "human_climate_only_"
    ) |>
    dplyr::rename(
      human_climate_only_total_adjusted_r_squared =
        dplyr::all_of("total_adjusted_r_squared")
    )
  assertthat::assert_that(
    all(c("human", "climate", "time") %in% names(data_wide)),
    msg = "Every controlled model must contain human, climate, and time."
  )

  data_records <-
    data_wide |>
    dplyr::left_join(
      data_human_climate_only,
      by = c(id_col, "analysis")
    ) |>
    dplyr::mutate(
      model_id = stringr::str_c(
        .data[["analysis"]],
        .data[[id_col]],
        sep = "|"
      ),
      signed_difference =
        .data[["human"]] - .data[["climate"]],
      signed_balance =
        .data[["signed_difference"]] /
          .data[["total_adjusted_r_squared"]],
      zero_human = pmax(.data[["human"]], 0),
      zero_climate = pmax(.data[["climate"]], 0),
      zero_time = pmax(.data[["time"]], 0),
      human_climate_total =
        .data[["zero_human"]] + .data[["zero_climate"]],
      three_group_total =
        .data[["human_climate_total"]] + .data[["zero_time"]],
      zero_balance = dplyr::if_else(
        .data[["human_climate_total"]] > 0,
        (.data[["zero_human"]] - .data[["zero_climate"]]) /
          .data[["human_climate_total"]],
        NA_real_
      ),
      human_allocation = dplyr::if_else(
        .data[["three_group_total"]] > 0,
        .data[["zero_human"]] / .data[["three_group_total"]],
        NA_real_
      ),
      climate_allocation = dplyr::if_else(
        .data[["three_group_total"]] > 0,
        .data[["zero_climate"]] / .data[["three_group_total"]],
        NA_real_
      ),
      time_allocation = dplyr::if_else(
        .data[["three_group_total"]] > 0,
        .data[["zero_time"]] / .data[["three_group_total"]],
        NA_real_
      ),
      signed_weight = abs(.data[["total_adjusted_r_squared"]]),
      zero_weight = .data[["human_climate_total"]],
      human_climate_only_signed_weight =
        abs(.data[["human_climate_only_total_adjusted_r_squared"]]),
      human_climate_only_signed_balance =
        (
          .data[["human_climate_only_human"]] -
            .data[["human_climate_only_climate"]]
        ) /
          .data[["human_climate_only_total_adjusted_r_squared"]],
      human_climate_only_zero_human = pmax(
        .data[["human_climate_only_human"]],
        0
      ),
      human_climate_only_zero_climate = pmax(
        .data[["human_climate_only_climate"]],
        0
      ),
      human_climate_only_zero_weight =
        .data[["human_climate_only_zero_human"]] +
          .data[["human_climate_only_zero_climate"]],
      human_climate_only_zero_balance = dplyr::if_else(
        .data[["human_climate_only_zero_weight"]] > 0,
        (
          .data[["human_climate_only_zero_human"]] -
            .data[["human_climate_only_zero_climate"]]
        ) / .data[["human_climate_only_zero_weight"]],
        NA_real_
      ),
      signed_ranking = dplyr::case_when(
        .data[["signed_difference"]] > 0 ~ "human",
        .data[["signed_difference"]] < 0 ~ "climate",
        .default = "tie"
      ),
      zero_ranking = dplyr::case_when(
        .data[["zero_balance"]] > 0 ~ "human",
        .data[["zero_balance"]] < 0 ~ "climate",
        is.finite(.data[["zero_balance"]]) ~ "tie",
        .default = NA_character_
      )
    ) |>
    dplyr::left_join(
      data_status |>
        dplyr::select(dplyr::all_of(required_status)),
      by = c(id_col, "analysis")
    ) |>
    dplyr::left_join(
      data_meta |>
        dplyr::select(dplyr::all_of(required_meta)),
      by = id_col
    )

  validate_spatial_coordinates(
    data_source = data_records,
    id_col = "model_id",
    group_col = "region"
  )

  return(data_records)
}
