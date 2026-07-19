testthat::test_that("summarise_prediction_draws averages within draws", {
  mat_draws <-
    matrix(
      c(
        1, 3, 10, 14,
        2, 4, 12, 16
      ),
      nrow = 2,
      byrow = TRUE
    )
  data_new <-
    tibble::tibble(
      age = c(0, 0, 500, 500),
      dataset_id = c("d1", "d2", "d1", "d2")
    )

  result <-
    summarise_prediction_draws(
      mat_draws = mat_draws,
      data_new = data_new,
      group_var = "dataset_id",
      probs = c(0.25, 0.75)
    )

  testthat::expect_identical(nrow(result), 2L)
  testthat::expect_equal(result[["estimate"]], c(2.5, 13))
  testthat::expect_equal(result[["estimate_error"]], c(sqrt(0.5), sqrt(2)))
  testthat::expect_equal(result[["conf_low"]], c(2.25, 12.5))
  testthat::expect_equal(result[["conf_high"]], c(2.75, 13.5))
  testthat::expect_identical(
    result[["n_datasets_marginalised"]],
    c(2L, 2L)
  )
})

testthat::test_that("summarise_prediction_draws validates dimensions", {
  testthat::expect_error(
    summarise_prediction_draws(
      mat_draws = matrix(1:4, nrow = 2),
      data_new = tibble::tibble(
        age = 0,
        dataset_id = "d1"
      ),
      group_var = "dataset_id"
    ),
    regexp = "match"
  )
})

testthat::test_that("summarise_prediction_draws retains prediction rows", {
  mat_draws <-
    matrix(
      c(1, 10, 3, 14),
      nrow = 2,
      byrow = TRUE
    )
  data_new <-
    tibble::tibble(
      age = c(0, 500),
      dataset_id = c("d1", "d1")
    )

  result <-
    summarise_prediction_draws(
      mat_draws = mat_draws,
      data_new = data_new,
      group_var = NULL,
      probs = c(0.25, 0.75)
    )

  testthat::expect_identical(nrow(result), 2L)
  testthat::expect_equal(result[["estimate"]], c(2, 12))
  testthat::expect_identical(result[["dataset_id"]], c("d1", "d1"))
  testthat::expect_identical(
    result[["n_datasets_marginalised"]],
    c(1L, 1L)
  )
})
