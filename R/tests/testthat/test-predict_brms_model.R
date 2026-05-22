testthat::test_that("predict_brms_model applies extra exp transform for log link", {
  testthat::skip_if_not_installed("ggeffects")
  testthat::skip_if_not_installed("insight")

  data_fit <-
    data.frame(
      age = 1:10,
      y = c(2, 3, 4, 6, 8, 10, 12, 13, 15, 18)
    )

  mod <-
    stats::glm(
      y ~ age,
      family = poisson(link = "log"),
      data = data_fit
    )

  baseline <-
    ggeffects::predict_response(
      model = mod,
      terms = "age",
      margin = "marginalmeans",
      back_transform = TRUE
    ) %>%
    as.data.frame()

  result <-
    predict_brms_model(mod)

  testthat::expect_equal(result[["age"]], baseline[["x"]])
  testthat::expect_equal(result[["value"]], exp(baseline[["predicted"]]))
  testthat::expect_equal(result[["conf.low"]], exp(baseline[["conf.low"]]))
  testthat::expect_equal(result[["conf.high"]], exp(baseline[["conf.high"]]))
})

testthat::test_that("predict_brms_model keeps scale for identity link", {
  testthat::skip_if_not_installed("ggeffects")
  testthat::skip_if_not_installed("insight")

  data_fit <-
    data.frame(
      age = 1:10,
      y = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6)
    )

  mod <-
    stats::glm(
      y ~ age,
      family = gaussian(link = "identity"),
      data = data_fit
    )

  baseline <-
    ggeffects::predict_response(
      model = mod,
      terms = "age",
      margin = "marginalmeans",
      back_transform = TRUE
    ) %>%
    as.data.frame()

  result <-
    predict_brms_model(mod)

  testthat::expect_equal(result[["age"]], baseline[["x"]])
  testthat::expect_equal(result[["value"]], baseline[["predicted"]])
  testthat::expect_equal(result[["conf.low"]], baseline[["conf.low"]])
  testthat::expect_equal(result[["conf.high"]], baseline[["conf.high"]])
})

testthat::test_that("predict_brms_model validates model input", {
  testthat::expect_error(
    predict_brms_model(NULL),
    regexp = "must not be NULL"
  )
})