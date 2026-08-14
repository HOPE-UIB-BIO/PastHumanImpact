testthat::test_that("overall importance plot validates input contracts", {
  testthat::expect_error(
    plot_hvarpart_adjr2_importance_overall(
      data_values = tibble::tibble(),
      data_statistics = tibble::tibble()
    ),
    "overall plot contract"
  )
})
