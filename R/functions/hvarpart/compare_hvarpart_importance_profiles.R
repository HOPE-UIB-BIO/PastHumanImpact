#' @title Compare HVarPart importance profiles
#' @description
#' Computes signed, exact zero-truncated, and negative-exclusion HVarPart
#' summaries. Differences retain the signed profile as an unmodified numerical
#' reference, while the zero-truncated profile is used in main figures.
#' @param data_importance Predictor-level output from
#' `get_hvarpart_importance()`.
#' @param group_vars Character vector of columns defining reported groups.
#' @return
#' A tibble containing all profile summaries and `delta_from_signed`.
#' @examples
#' \dontrun{
#' compare_hvarpart_importance_profiles(
#'   data_importance = importance,
#'   group_vars = "analysis"
#' )
#' }
compare_hvarpart_importance_profiles <- function(
  data_importance,
  group_vars
) {
  assertthat::assert_that(
    is.data.frame(data_importance),
    is.character(group_vars),
    length(group_vars) > 0L,
    all(group_vars %in% names(data_importance)),
    msg = "Profile comparison inputs must contain valid grouping columns."
  )

  vec_profiles <- c(
    "signed",
    "zero_truncated",
    "exclude_negative"
  )

  data_profiles <-
    vec_profiles |>
    purrr::map(
      ~ summarise_hvarpart_importance(
        data_importance = data_importance,
        group_vars = group_vars,
        profile = .x
      )
    ) |>
    dplyr::bind_rows()

  data_signed <-
    data_profiles |>
    dplyr::filter(.data[["profile"]] == "signed") |>
    dplyr::select(
      dplyr::all_of(c(group_vars, "predictor")),
      signed_allocation = pooled_allocation
    )

  data_comparison <-
    data_profiles |>
    dplyr::left_join(
      data_signed,
      by = c(group_vars, "predictor")
    ) |>
    dplyr::mutate(
      delta_from_signed = .data[["pooled_allocation"]] -
        .data[["signed_allocation"]]
    )

  return(data_comparison)
}
