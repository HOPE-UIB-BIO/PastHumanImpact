testthat::test_that("variance decomposition summary preserves groups", {
  data_decomposition <-
    tibble::tibble(
      model_id = c("one", "two"),
      group = "test",
      has_finite_decomposition = c(TRUE, FALSE),
      accounting_within_tolerance = c(TRUE, FALSE),
      total_adjusted_r_squared = c(0.2, NA_real_),
      unique_human = c(0.1, NA_real_),
      unique_climate = c(0.05, NA_real_),
      shared = c(0.05, NA_real_),
      unexplained = c(0.8, NA_real_),
      bounded_total_adjusted_r_squared = c(0.2, NA_real_),
      bounded_unique_human = c(0.1, NA_real_),
      bounded_unique_climate = c(0.05, NA_real_),
      bounded_shared = c(0.05, NA_real_),
      bounded_unexplained = c(0.8, NA_real_)
    )

  result <-
    summarise_hvarpart_variance_decomposition(
      data_decomposition = data_decomposition,
      group_vars = "group"
    )

  testthat::expect_identical(result[["n_models"]], 2L)

  testthat::expect_identical(result[["n_available"]], 1L)

  testthat::expect_equal(result[["unique_human_mean"]], 0.1)
})

testthat::test_that("variance decomposition summary validates columns", {
  testthat::expect_error(
    summarise_hvarpart_variance_decomposition(
      data_decomposition = tibble::tibble(),
      group_vars = "group"
    ),
    "summary contract"
  )
})
