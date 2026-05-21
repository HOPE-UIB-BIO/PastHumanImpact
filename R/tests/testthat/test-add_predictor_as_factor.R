testthat::test_that("add_predictor_as_factor() constrains factor levels", {
  data_input <-
    data.frame(
      predictor = c("human", "climate", "human")
    )

  res_data <-
    add_predictor_as_factor(data_input)

  vec_predictor <-
    dplyr::pull(res_data, predictor)

  testthat::expect_s3_class(vec_predictor, "factor")

  testthat::expect_identical(
    as.character(vec_predictor),
    c("human", "climate", "human")
  )

  testthat::expect_identical(
    levels(vec_predictor),
    c("human", "climate")
  )
})

testthat::test_that("add_predictor_as_factor() preserves NA values", {
  data_input <-
    data.frame(
      predictor = c("human", NA_character_, "climate")
    )

  res_data <-
    add_predictor_as_factor(data_input)

  vec_predictor <-
    dplyr::pull(res_data, predictor)

  testthat::expect_s3_class(vec_predictor, "factor")
  testthat::expect_true(is.na(vec_predictor[[2]]))
})

testthat::test_that("add_predictor_as_factor() returns expected column count", {
  data_input <-
    data.frame(
      predictor = c("human", "climate"),
      value     = c(1.0, 2.0)
    )

  res_data <-
    add_predictor_as_factor(data_input)

  testthat::expect_equal(ncol(res_data), 2L)
  testthat::expect_true("predictor" %in% names(res_data))
  testthat::expect_true("value" %in% names(res_data))
})