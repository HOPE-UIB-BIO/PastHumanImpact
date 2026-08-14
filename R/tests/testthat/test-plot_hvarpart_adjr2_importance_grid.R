testthat::test_that("importance grid validates input contracts", {
  testthat::expect_error(
    plot_hvarpart_adjr2_importance_grid(
      data_values = tibble::tibble(),
      data_statistics = tibble::tibble(),
      x_limits = c(0, 1),
      y_limits = c(0, 1)
    ),
    "grid plot contract"
  )
})
