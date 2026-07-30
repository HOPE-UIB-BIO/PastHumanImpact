#' @title Summarise HVarPart model eligibility
#' @description
#' Collapse canonical predictor-level HVarPart results to one row per model,
#' then count eligibility, exclusions, and negative contributions by reporting
#' group without double-counting predictor rows.
#' @param data_importance Predictor-level output from
#' `get_hvarpart_importance()`.
#' @param group_vars Character vector of columns defining reported groups.
#' @return
#' A tibble containing model counts, eligibility counts, exclusion-reason
#' counts, and negative-contribution counts.
summarise_hvarpart_audit <- function(
  data_importance,
  group_vars
) {
  required_columns <-
    c(
      "model_id",
      "is_importance_eligible",
      "exclusion_reason",
      "has_negative_unique",
      "has_negative_individual"
    )

  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_columns %in% names(data_importance)),
    msg = "`data_importance` must contain canonical HVarPart diagnostics."
  )
  assertthat::assert_that(
    is.character(group_vars),
    length(group_vars) > 0L,
    !anyNA(group_vars),
    !anyDuplicated(group_vars),
    all(group_vars %in% names(data_importance)),
    msg = "`group_vars` must name one or more grouping columns."
  )

  data_models <-
    data_importance |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(group_vars, "model_id")
        )
      )
    ) |>
    dplyr::summarise(
      is_importance_eligible = dplyr::first(
        .data[["is_importance_eligible"]]
      ),
      exclusion_reason = dplyr::first(
        .data[["exclusion_reason"]]
      ),
      has_negative_unique = dplyr::first(
        .data[["has_negative_unique"]]
      ),
      has_negative_individual = dplyr::first(
        .data[["has_negative_individual"]]
      ),
      .groups = "drop"
    )

  data_audit <-
    data_models |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      n_models = dplyr::n(),
      n_eligible = sum(.data[["is_importance_eligible"]]),
      n_excluded = sum(!.data[["is_importance_eligible"]]),
      n_missing_result = sum(
        .data[["exclusion_reason"]] == "missing_result",
        na.rm = TRUE
      ),
      n_missing_predictor = sum(
        .data[["exclusion_reason"]] == "missing_predictor",
        na.rm = TRUE
      ),
      n_non_finite_total = sum(
        .data[["exclusion_reason"]] == "non_finite_total",
        na.rm = TRUE
      ),
      n_non_positive_total = sum(
        .data[["exclusion_reason"]] == "non_positive_total",
        na.rm = TRUE
      ),
      n_non_finite_individual = sum(
        .data[["exclusion_reason"]] == "non_finite_individual",
        na.rm = TRUE
      ),
      n_negative_unique = sum(.data[["has_negative_unique"]]),
      n_negative_individual = sum(
        .data[["has_negative_individual"]]
      ),
      .groups = "drop"
    )

  return(data_audit)
}
