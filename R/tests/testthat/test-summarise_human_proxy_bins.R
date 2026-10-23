testthat::test_that("summarise_human_proxy_bins() preserves tied values", {
  matched <-
    tidyr::expand_grid(
      dataset_id = letters[1:5],
      age_bp = c(2000, 2500, 3000)
    ) |>
    dplyr::mutate(
      region = "Europe",
      spd_transformed = rep(c(0, 0, 1), each = 5),
      kk10_transformed = .data[["spd_transformed"]] + 1,
      hyde_transformed = .data[["spd_transformed"]] + 2
    )

  result <-
    summarise_human_proxy_bins(matched, bin_count = 10L)

  observed <-
    result |>
    dplyr::filter(.data[["scope_type"]] == "overall") |>
    dplyr::pull(.data[["bin_count_observed"]]) |>
    unique()

  testthat::expect_true(observed < 10L)
  testthat::expect_true(all(result[["range_025"]] <= result[["median"]]))
  testthat::expect_true(all(result[["median"]] <= result[["range_975"]]))
})
