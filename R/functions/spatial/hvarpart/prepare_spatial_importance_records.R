#' @title Prepare dataset-level HVarPart balances for spatial analysis
#' @description
#' Convert paired human and climate HVarPart components into signed and exact
#' zero-truncated dataset-level balances with the denominators used as weights.
#' @param data_importance Canonical output from `compute_hvarpart_importance()`.
#' @param data_meta Site metadata containing identifiers and spatial fields.
#' @param id_col Identifier shared by importance and metadata tables.
#' @return
#' One row per eligible model with signed and zero-truncated balances, weights,
#' coordinates, region, climate zone, and ranking directions.
#' @examples
#' \dontrun{
#' prepare_spatial_importance_records(
#'   data_importance = h1_importance,
#'   data_meta = metadata
#' )
#' }
prepare_spatial_importance_records <- function(
  data_importance,
  data_meta,
  id_col = "dataset_id"
) {
  required_importance <-
    c(
      id_col,
      "model_id",
      "predictor",
      "individual",
      "total_adjusted_r_squared",
      "is_importance_eligible"
    )
  required_meta <-
    c(id_col, "long", "lat", "region", "climatezone")
  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_importance %in% names(data_importance)),
    is.data.frame(data_meta),
    all(required_meta %in% names(data_meta)),
    msg = "Spatial importance inputs do not satisfy the required contract."
  )

  data_pairs <-
    data_importance |>
    dplyr::filter(
      .data[["is_importance_eligible"]],
      .data[["predictor"]] %in% c("human", "climate")
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        id_col,
        "model_id",
        "predictor",
        "individual",
        "total_adjusted_r_squared"
      ))
    )

  duplicate_rows <-
    data_pairs |>
    dplyr::count(
      .data[["model_id"]],
      .data[["predictor"]],
      name = "n_rows"
    ) |>
    dplyr::filter(.data[["n_rows"]] != 1L)
  assertthat::assert_that(
    nrow(duplicate_rows) == 0L,
    msg = "Each eligible model must have one human and one climate row."
  )

  data_wide <-
    data_pairs |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(
        id_col,
        "model_id",
        "total_adjusted_r_squared"
      )),
      names_from = "predictor",
      values_from = "individual"
    )
  assertthat::assert_that(
    all(c("human", "climate") %in% names(data_wide)),
    all(is.finite(data_wide[["human"]])),
    all(is.finite(data_wide[["climate"]])),
    all(data_wide[["total_adjusted_r_squared"]] > 0),
    msg = "Eligible models must contain finite human and climate components."
  )

  data_records <-
    data_wide |>
    dplyr::mutate(
      signed_weight = .data[["total_adjusted_r_squared"]],
      signed_balance =
        (.data[["human"]] - .data[["climate"]]) /
        .data[["signed_weight"]],
      zero_human = pmax(.data[["human"]], 0),
      zero_climate = pmax(.data[["climate"]], 0),
      zero_weight = .data[["zero_human"]] + .data[["zero_climate"]],
      zero_balance = dplyr::if_else(
        .data[["zero_weight"]] > 0,
        (.data[["zero_human"]] - .data[["zero_climate"]]) /
          .data[["zero_weight"]],
        NA_real_
      ),
      signed_ranking = dplyr::case_when(
        .data[["signed_balance"]] > 0 ~ "human",
        .data[["signed_balance"]] < 0 ~ "climate",
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
