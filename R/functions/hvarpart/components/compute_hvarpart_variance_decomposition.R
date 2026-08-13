#' @title Compute model-level HVarPart variance decompositions
#' @description
#' Collapse canonical predictor-level HVarPart output to one signed
#' commonality decomposition per model, with a bounded zero-truncated
#' sensitivity profile and explicit accounting diagnostics.
#' @param data_importance Output from `compute_hvarpart_importance()`.
#' @param id_cols Character vector of model identifier columns to retain.
#' @param tolerance Maximum absolute accounting discrepancy.
#' @return
#' A tibble with one row per model. Signed direct `Var.part` fractions are
#' primary. Columns prefixed `bounded_` form a zero-truncated sensitivity
#' rescaled to the bounded explained fraction.
compute_hvarpart_variance_decomposition <- function(
  data_importance,
  id_cols,
  tolerance = 0.001
) {
  required_columns <-
    c(
      "model_id",
      "predictor",
      "individual",
      "varpart_unique",
      "shared",
      "varpart_total",
      "total_adjusted_r_squared",
      "model_result_available",
      "varpart_available",
      "has_finite_varpart"
    )

  assertthat::assert_that(
    is.data.frame(data_importance),
    all(c(required_columns, id_cols) %in% names(data_importance)),
    msg = "`data_importance` does not satisfy the decomposition contract."
  )
  assertthat::assert_that(
    is.character(id_cols),
    length(id_cols) > 0L,
    !anyNA(id_cols),
    !anyDuplicated(id_cols),
    msg = "`id_cols` must name one or more unique identifier columns."
  )
  assertthat::assert_that(
    is.numeric(tolerance),
    length(tolerance) == 1L,
    is.finite(tolerance),
    tolerance > 0,
    msg = "`tolerance` must be one finite positive number."
  )

  duplicate_rows <-
    data_importance |>
    dplyr::count(
      dplyr::across(dplyr::all_of(c(id_cols, "predictor"))),
      name = "n_rows"
    ) |>
    dplyr::filter(.data[["n_rows"]] > 1L)

  assertthat::assert_that(
    nrow(duplicate_rows) == 0L,
    msg = "Each model and predictor must occur exactly once."
  )

  expected_predictors <-
    c(
      "human",
      "climate"
    )

  data_models <-
    data_importance |>
    dplyr::filter(.data[["predictor"]] %in% expected_predictors) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(id_cols))
    ) |>
    dplyr::summarise(
      has_required_predictors =
        all(expected_predictors %in% .data[["predictor"]]),
      model_result_available =
        all(.data[["model_result_available"]]),
      varpart_available = all(.data[["varpart_available"]]),
      has_finite_varpart = all(.data[["has_finite_varpart"]]),
      total_adjusted_r_squared =
        dplyr::first(.data[["total_adjusted_r_squared"]]),
      varpart_total = dplyr::first(.data[["varpart_total"]]),
      unique_human = .data[["varpart_unique"]][
        match("human", .data[["predictor"]])
      ],
      unique_climate = .data[["varpart_unique"]][
        match("climate", .data[["predictor"]])
      ],
      shared = dplyr::first(.data[["shared"]]),
      human_individual_importance = .data[["individual"]][
        match("human", .data[["predictor"]])
      ],
      climate_individual_importance = .data[["individual"]][
        match("climate", .data[["predictor"]])
      ],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      unexplained = 1 - .data[["total_adjusted_r_squared"]],
      explained_component_sum =
        .data[["unique_human"]] +
        .data[["unique_climate"]] +
        .data[["shared"]],
      accounting_residual =
        .data[["total_adjusted_r_squared"]] -
        .data[["explained_component_sum"]],
      full_component_sum =
        .data[["explained_component_sum"]] +
        .data[["unexplained"]],
      full_accounting_residual = 1 - .data[["full_component_sum"]],
      varpart_total_residual =
        .data[["total_adjusted_r_squared"]] -
        .data[["varpart_total"]],
      has_finite_decomposition =
        .data[["has_required_predictors"]] &
        .data[["model_result_available"]] &
        .data[["varpart_available"]] &
        .data[["has_finite_varpart"]] &
        is.finite(.data[["total_adjusted_r_squared"]]) &
        is.finite(.data[["unexplained"]]),
      accounting_within_tolerance =
        .data[["has_finite_decomposition"]] &
        abs(.data[["accounting_residual"]]) <= tolerance &
        abs(.data[["full_accounting_residual"]]) <= tolerance &
        abs(.data[["varpart_total_residual"]]) <= tolerance,
      has_negative_unique_human =
        is.finite(.data[["unique_human"]]) &
        .data[["unique_human"]] < 0,
      has_negative_unique_climate =
        is.finite(.data[["unique_climate"]]) &
        .data[["unique_climate"]] < 0,
      has_negative_shared =
        is.finite(.data[["shared"]]) &
        .data[["shared"]] < 0,
      has_negative_unexplained =
        is.finite(.data[["unexplained"]]) &
        .data[["unexplained"]] < 0,
      bounded_total_adjusted_r_squared = pmin(
        pmax(.data[["total_adjusted_r_squared"]], 0),
        1
      ),
      positive_component_sum =
        pmax(.data[["unique_human"]], 0) +
        pmax(.data[["unique_climate"]], 0) +
        pmax(.data[["shared"]], 0),
      bounded_scale = dplyr::case_when(
        !.data[["has_finite_decomposition"]] ~ NA_real_,
        .data[["positive_component_sum"]] > 0 ~
          .data[["bounded_total_adjusted_r_squared"]] /
            .data[["positive_component_sum"]],
        .data[["bounded_total_adjusted_r_squared"]] == 0 ~ 0,
        .default = NA_real_
      ),
      bounded_unique_human =
        pmax(.data[["unique_human"]], 0) * .data[["bounded_scale"]],
      bounded_unique_climate =
        pmax(.data[["unique_climate"]], 0) * .data[["bounded_scale"]],
      bounded_shared =
        pmax(.data[["shared"]], 0) * .data[["bounded_scale"]],
      bounded_unexplained =
        1 - .data[["bounded_total_adjusted_r_squared"]],
      bounded_component_sum =
        .data[["bounded_unique_human"]] +
        .data[["bounded_unique_climate"]] +
        .data[["bounded_shared"]] +
        .data[["bounded_unexplained"]],
      bounded_accounting_residual =
        1 - .data[["bounded_component_sum"]]
    ) |>
    dplyr::select(-dplyr::all_of(c("positive_component_sum", "bounded_scale")))

  return(data_models)
}
