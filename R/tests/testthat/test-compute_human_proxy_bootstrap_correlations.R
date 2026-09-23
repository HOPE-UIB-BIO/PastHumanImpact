testthat::test_that(
  "compute_human_proxy_bootstrap_correlations() is deterministic",
  {
  matched <-
    tidyr::expand_grid(
      dataset_id = letters[1:8],
      age_bp = seq(2000, 5500, by = 500)
    ) |>
    dplyr::mutate(
      region = "Europe",
      spd_transformed = as.numeric(.data[["age_bp"]]) +
        as.numeric(factor(.data[["dataset_id"]])),
      kk10_transformed = .data[["spd_transformed"]] * 2,
      hyde_transformed = .data[["spd_transformed"]] * 3
    )

  first <-
    compute_human_proxy_bootstrap_correlations(
      matched,
      repetitions = 5L,
      seed = 42L,
      minimum_bins = 3L,
      minimum_datasets = 1L,
      minimum_ages = 1L
    )

  second <-
    compute_human_proxy_bootstrap_correlations(
      matched,
      repetitions = 5L,
      seed = 42L,
      minimum_bins = 3L,
      minimum_datasets = 1L,
      minimum_ages = 1L
    )

  testthat::expect_identical(first, second)
  testthat::expect_true(all(first[["kendall_tau"]] == 1))
})
