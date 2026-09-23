#' @title Compute Kendall correlations between binned proxy medians
#' @description
#' Compute SPD-versus-KK10 and SPD-versus-HYDE Kendall rank correlations from
#' an exported long-form bin-summary contract.
#' @param data_bins Human-proxy bin summaries.
#' @param minimum_bins Minimum complete bins required to report a correlation.
#' @param minimum_datasets Minimum distinct datasets required in every bin.
#' @param minimum_ages Minimum distinct ages required in every bin.
#' @return Correlation table with coverage and eligibility fields.
#' @examples
#' \dontrun{
#' compute_human_proxy_correlations(data_bins)
#' }
compute_human_proxy_correlations <- function(
  data_bins,
  minimum_bins = 8L,
  minimum_datasets = 5L,
  minimum_ages = 3L
) {
  required_columns <-
    c(
      "scope_type",
      "scope",
      "bin_id",
      "proxy",
      "median",
      "n_rows",
      "n_datasets",
      "n_ages",
      "bin_count_requested",
      "bin_count_observed"
    )

  assertthat::assert_that(
    is.data.frame(data_bins),
    all(required_columns %in% names(data_bins)),
    all(c(minimum_bins, minimum_datasets, minimum_ages) >= 1L),
    msg = "Human-proxy correlation inputs are invalid."
  )

  data_wide <-
    data_bins |>
    dplyr::select(
      dplyr::all_of(
        c(
          "scope_type",
          "scope",
          "bin_id",
          "proxy",
          "median",
          "n_rows",
          "n_datasets",
          "n_ages",
          "bin_count_requested",
          "bin_count_observed"
        )
      )
    ) |>
    tidyr::pivot_wider(
      names_from = "proxy",
      values_from = "median"
    )

  res_correlations <-
    data_wide |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("kk10", "hyde")),
      names_to = "comparison_proxy",
      values_to = "comparison_median"
    ) |>
    dplyr::group_by(
      .data[["scope_type"]],
      .data[["scope"]],
      .data[["comparison_proxy"]],
      .data[["bin_count_requested"]]
    ) |>
    dplyr::summarise(
      matched_bins = sum(
        is.finite(.data[["spd"]]) &
          is.finite(.data[["comparison_median"]])
      ),
      minimum_bin_datasets = min(.data[["n_datasets"]]),
      minimum_bin_ages = min(.data[["n_ages"]]),
      eligible =
        .data[["matched_bins"]] >= minimum_bins &
          .data[["minimum_bin_datasets"]] >= minimum_datasets &
          .data[["minimum_bin_ages"]] >= minimum_ages,
      kendall_tau = dplyr::if_else(
        .data[["eligible"]],
        stats::cor(
          .data[["spd"]],
          .data[["comparison_median"]],
          method = "kendall",
          use = "complete.obs"
        ),
        NA_real_
      ),
      .groups = "drop"
    )

  return(res_correlations)
}
