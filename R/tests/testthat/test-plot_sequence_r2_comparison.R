testthat::test_that("plot_sequence_r2_comparison() returns four facets", {
  comparison <- tidyr::expand_grid(
    dataset_id = c("a", "b"),
    metric = factor(c(
      "Total adjusted R²", "Pure human adjusted R²",
      "Pure climate adjusted R²", "Pure time adjusted R²"
    ))
  ) |>
    dplyr::mutate(spd_only = 0.1, filtered_joint = 0.2, difference = 0.1)
  summary <- comparison |>
    dplyr::summarise(
      n_sequences = dplyr::n(), correlation = 1,
      median_difference = 0.1, .by = "metric"
    )
  result <- plot_sequence_r2_comparison(comparison, summary)
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(
    dplyr::n_distinct(dplyr::pull(comparison, metric)),
    4L
  )
})

testthat::test_that("plot_sequence_r2_comparison() validates inputs", {
  testthat::expect_error(
    plot_sequence_r2_comparison(data.frame(x = 1), data.frame(x = 1)),
    "contract"
  )
})
