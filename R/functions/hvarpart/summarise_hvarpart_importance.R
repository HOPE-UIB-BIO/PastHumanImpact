#' @title Summarise HVarPart predictor importance
#' @description
#' Pool model-level HVarPart individual contributions using a documented
#' signed or sensitivity profile.
#' @param data_importance Predictor-level output from
#' `get_hvarpart_importance()`.
#' @param group_vars Character vector of columns defining reported groups.
#' @param profile One of `signed`, `zero_truncated`, or `exclude_negative`.
#' @return
#' A tibble with one row per group and predictor containing the pooled
#' allocation, numerator, denominator, and contributing model count.
#' @details
#' The primary `signed` profile uses
#' `sum(individual) / sum(total_adjusted_r_squared)`. The
#' `zero_truncated` profile replaces negative individual contributions with
#' exact zero and recomputes each model denominator from those truncated
#' contributions. The `exclude_negative` profile removes a complete model if
#' any expected predictor has a negative individual contribution.
summarise_hvarpart_importance <- function(
  data_importance,
  group_vars,
  profile = c(
    "signed",
    "zero_truncated",
    "exclude_negative"
  )
) {
  required_columns <-
    c(
      "model_id",
      "predictor",
      "individual",
      "total_adjusted_r_squared",
      "has_negative_individual",
      "is_importance_eligible"
    )

  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_columns %in% names(data_importance)),
    msg = "`data_importance` must contain canonical HVarPart columns."
  )
  assertthat::assert_that(
    is.character(group_vars),
    length(group_vars) > 0L,
    !anyNA(group_vars),
    !anyDuplicated(group_vars),
    all(group_vars %in% names(data_importance)),
    msg = "`group_vars` must name one or more grouping columns."
  )

  profile <- match.arg(profile)

  duplicate_rows <-
    data_importance |>
    dplyr::count(
      .data[["model_id"]],
      .data[["predictor"]],
      name = "n_rows"
    ) |>
    dplyr::filter(.data[["n_rows"]] > 1L)

  assertthat::assert_that(
    nrow(duplicate_rows) == 0L,
    msg = "Each model and predictor must occur exactly once."
  )

  data_profile <-
    data_importance |>
    dplyr::filter(.data[["is_importance_eligible"]])

  if (
    profile == "exclude_negative"
  ) {
    data_profile <-
      data_profile |>
      dplyr::filter(!.data[["has_negative_individual"]])
  }

  if (
    profile == "zero_truncated"
  ) {
    data_profile <-
      data_profile |>
      dplyr::mutate(
        profile_individual = pmax(
          .data[["individual"]],
          0
        )
      ) |>
      dplyr::group_by(.data[["model_id"]]) |>
      dplyr::mutate(
        profile_total = sum(.data[["profile_individual"]])
      ) |>
      dplyr::ungroup()
  } else {
    data_profile <-
      data_profile |>
      dplyr::mutate(
        profile_individual = .data[["individual"]],
        profile_total = .data[["total_adjusted_r_squared"]]
      )
  }

  data_summary <-
    data_profile |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(group_vars, "predictor")
        )
      )
    ) |>
    dplyr::summarise(
      profile = profile,
      individual_sum = sum(.data[["profile_individual"]]),
      total_sum = sum(.data[["profile_total"]]),
      pooled_allocation = .data[["individual_sum"]] /
        .data[["total_sum"]],
      n_models = dplyr::n_distinct(.data[["model_id"]]),
      .groups = "drop"
    ) |>
    dplyr::relocate(
      dplyr::all_of(c(group_vars, "predictor", "profile"))
    )

  return(data_summary)
}
