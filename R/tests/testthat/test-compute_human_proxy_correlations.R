testthat::test_that(
  "compute_human_proxy_correlations() recovers rank agreement",
  {
  bins <-
    tidyr::expand_grid(
      scope_type = "overall",
      scope = "Overall",
      bin_id = 1:10,
      proxy = c("spd", "kk10", "hyde")
    ) |>
    dplyr::mutate(
      median = dplyr::case_when(
        .data[["proxy"]] == "hyde" ~ 11 - .data[["bin_id"]],
        .default = as.numeric(.data[["bin_id"]])
      ),
      n_rows = 20L,
      n_datasets = 10L,
      n_ages = 5L,
      bin_count_requested = 10L,
      bin_count_observed = 10L
    )

  result <-
    compute_human_proxy_correlations(bins)

  testthat::expect_equal(
    result[["kendall_tau"]][result[["comparison_proxy"]] == "kk10"],
    1
  )
  testthat::expect_equal(
    result[["kendall_tau"]][result[["comparison_proxy"]] == "hyde"],
    -1
  )
})
