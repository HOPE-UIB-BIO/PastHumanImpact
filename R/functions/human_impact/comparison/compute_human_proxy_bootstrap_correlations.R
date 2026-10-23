#' @title Bootstrap human-proxy Kendall correlations by dataset
#' @description
#' Resample complete pollen-record histories, reconstruct SPD-defined bins, and
#' return percentile intervals for eligible Kendall correlations.
#' @param data_matched Matched human-proxy observations.
#' @param bin_count Requested number of SPD bins.
#' @param repetitions Number of bootstrap replicates.
#' @param seed Integer random seed.
#' @param minimum_bins Minimum complete bins required.
#' @param minimum_datasets Minimum datasets required in every bin.
#' @param minimum_ages Minimum ages required in every bin.
#' @return Correlation table with bootstrap limits and successful replicate
#'   counts.
#' @examples
#' \dontrun{
#' compute_human_proxy_bootstrap_correlations(data_matched, repetitions = 100L)
#' }
compute_human_proxy_bootstrap_correlations <- function(
  data_matched,
  bin_count = 10L,
  repetitions = 2000L,
  seed = 1234L,
  minimum_bins = 8L,
  minimum_datasets = 5L,
  minimum_ages = 3L
) {
  assertthat::assert_that(
    is.data.frame(data_matched),
    "dataset_id" %in% names(data_matched),
    is.numeric(repetitions),
    length(repetitions) == 1L,
    repetitions == as.integer(repetitions),
    repetitions >= 1L,
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Human-proxy bootstrap inputs are invalid."
  )

  data_bins <-
    summarise_human_proxy_bins(
      data_matched = data_matched,
      bin_count = bin_count,
      include_regions = TRUE
    )

  data_point_estimates <-
    compute_human_proxy_correlations(
      data_bins = data_bins,
      minimum_bins = minimum_bins,
      minimum_datasets = minimum_datasets,
      minimum_ages = minimum_ages
    )

  vec_dataset_ids <-
    unique(data_matched[["dataset_id"]])

  vec_dataset_index <-
    match(data_matched[["dataset_id"]], vec_dataset_ids)

  set.seed(seed)

  list_bootstrap <-
    seq_len(as.integer(repetitions)) |>
    purrr::map(
      .f = ~ {
        vec_sampled_positions <-
          sample(
            seq_along(vec_dataset_ids),
            size = length(vec_dataset_ids),
            replace = TRUE
          )

        vec_dataset_multiplicity <-
          tabulate(
            vec_sampled_positions,
            nbins = length(vec_dataset_ids)
          )

        vec_retained_rows <-
          which(vec_dataset_multiplicity[vec_dataset_index] > 0L)
        vec_row_multiplicity <-
          vec_dataset_multiplicity[vec_dataset_index[vec_retained_rows]]
        vec_bootstrap_rows <-
          rep(vec_retained_rows, times = vec_row_multiplicity)
        vec_copy_index <- sequence(vec_row_multiplicity)

        data_bootstrap <-
          data_matched[vec_bootstrap_rows, , drop = FALSE]
        data_bootstrap[["dataset_id"]] <-
          stringr::str_c(
            data_matched[["dataset_id"]][vec_bootstrap_rows],
            "__",
            vec_copy_index
          )

        data_bootstrap |>
          summarise_human_proxy_bins(
            bin_count = bin_count,
            include_regions = TRUE
          ) |>
          compute_human_proxy_correlations(
            minimum_bins = minimum_bins,
            minimum_datasets = minimum_datasets,
            minimum_ages = minimum_ages
          ) |>
          dplyr::mutate(replicate = .x)
      }
    )

  data_bootstrap_results <-
    dplyr::bind_rows(list_bootstrap)

  data_intervals <-
    data_bootstrap_results |>
    dplyr::filter(is.finite(.data[["kendall_tau"]])) |>
    dplyr::group_by(
      .data[["scope_type"]],
      .data[["scope"]],
      .data[["comparison_proxy"]],
      .data[["bin_count_requested"]]
    ) |>
    dplyr::summarise(
      tau_bootstrap_025 = stats::quantile(
        .data[["kendall_tau"]],
        probs = 0.025,
        names = FALSE,
        type = 8
      ),
      tau_bootstrap_975 = stats::quantile(
        .data[["kendall_tau"]],
        probs = 0.975,
        names = FALSE,
        type = 8
      ),
      successful_replicates = dplyr::n(),
      .groups = "drop"
    )

  res_correlations <-
    data_point_estimates |>
    dplyr::left_join(
      data_intervals,
      by = c(
        "scope_type",
        "scope",
        "comparison_proxy",
        "bin_count_requested"
      ),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      bootstrap_repetitions = as.integer(repetitions),
      bootstrap_seed = as.integer(seed)
    )

  return(res_correlations)
}
