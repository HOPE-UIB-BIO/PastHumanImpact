testthat::test_that("scale_model_predictor() scales and retains x", {
  data_source <-
    tibble::tibble(age_ka = c(0, 1, 2))

  result <-
    scale_model_predictor(
      data_source = data_source,
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 1,
      x_sd = 1
    )

  testthat::expect_identical(result[["age_ka"]], c(0, 1, 2))
  testthat::expect_identical(result[["age_ka_scaled"]], c(-1, 0, 1))
})

testthat::test_that("scale_model_predictor() validates scaling", {
  data_source <-
    tibble::tibble(age_ka = c(0, 1))

  testthat::expect_error(
    scale_model_predictor(
      data_source = data_source,
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 0.5,
      x_sd = 0
    ),
    regexp = "positive"
  )

  testthat::expect_error(
    scale_model_predictor(
      data_source = tibble::tibble(age_ka = c(0, Inf)),
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 0.5,
      x_sd = 1
    ),
    regexp = "finite"
  )
})
