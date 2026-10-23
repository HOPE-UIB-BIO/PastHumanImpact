testthat::test_that("plot_human_proxy_convergence() returns a ggplot", {
  bins <-
    tidyr::expand_grid(
      scope_type = "overall",
      scope = "Overall",
      bin_id = 1:3,
      proxy = c("spd", "kk10", "hyde")
    ) |>
    dplyr::mutate(
      median = as.numeric(.data[["bin_id"]]),
      range_025 = .data[["median"]] - 0.1,
      range_975 = .data[["median"]] + 0.1,
      n_rows = 10L,
      n_datasets = 5L,
      n_ages = 3L,
      bin_count_requested = 3L,
      bin_count_observed = 3L
    )

  correlations <-
    tibble::tibble(
      scope_type = "overall",
      scope = "Overall",
      comparison_proxy = c("kk10", "hyde"),
      bin_count_requested = 3L,
      matched_bins = 3L,
      minimum_bin_datasets = 5L,
      minimum_bin_ages = 3L,
      eligible = TRUE,
      kendall_tau = 1
    )

  bins <-
    dplyr::bind_rows(
      bins,
      bins |>
        dplyr::mutate(scope_type = "region", scope = "Europe")
    )

  correlations <-
    dplyr::bind_rows(
      correlations,
      correlations |>
        dplyr::mutate(scope_type = "region", scope = "Europe")
    )

  result <-
    plot_human_proxy_convergence(bins, correlations)

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(unique(result$data$scope_type), "overall")
  testthat::expect_equal(unique(result$data$scope), "Overall")
  testthat::expect_equal(unique(result$layers[[5]]$data$scope), "Overall")

  result_region <-
    plot_human_proxy_convergence(
      bins,
      correlations,
      scope_type = "region"
    )

  testthat::expect_equal(unique(result_region$data$scope), "Europe")
  testthat::expect_true(
    all(stringr::str_starts(result_region$layers[[5]]$data$annotation, "Europe:"))
  )
})
