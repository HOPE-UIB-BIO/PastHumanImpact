testthat::test_that("predict_brms_model keeps response-scale log predictions", {
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
  testthat::expect_equal(result[["value"]], baseline[["predicted"]])
  testthat::expect_equal(result[["conf.low"]], baseline[["conf.low"]])
  testthat::expect_equal(result[["conf.high"]], baseline[["conf.high"]])
})

testthat::test_that("predict_brms_model averages datasets within draws", {
  testthat::skip_if_not_installed("brms")
  testthat::skip_if_not_installed("posterior")

  mod <-
    structure(list(), class = "brmsfit")
  data_new <-
    tibble::tibble(
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      dataset_id = factor(c("d1", "d2", "d1", "d2")),
      age = c(0, 0, 500, 500),
      age_ka = c(0, 0, 0.5, 0.5),
      age_ka_scaled = c(-1, -1, 0, 0)
    )
  model_config_row <-
    tibble::tibble(
      analysis = "pap_temporal",
      model_id = "pap_temporal__n0__Europe__Temperate",
      variable = "n0",
      group_var = "dataset_id",
      model_profile = "within_stratum_dataset_fs",
      model_file_name = "model_a.qs"
    )
  mat_expected <-
    matrix(
      c(
        1, 3, 10, 14,
        2, 4, 12, 16
      ),
      nrow = 2,
      byrow = TRUE
    )

  testthat::local_mocked_bindings(
    ndraws = function(...) 2L,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_epred = function(...) mat_expected,
    .package = "brms"
  )

  result <-
    predict_brms_model(
      mod = mod,
      newdata = data_new,
      model_config_row = model_config_row,
      max_prediction_draws = 2L,
      prediction_range = "group_observed"
    )

  testthat::expect_identical(nrow(result), 2L)
  testthat::expect_equal(result[["estimate"]], c(2.5, 13))
  testthat::expect_identical(
    result[["n_datasets_marginalised"]],
    c(2L, 2L)
  )
  testthat::expect_true(all(
    result[["prediction_estimand"]] == "equal_weighted_dataset_mean"
  ))
  testthat::expect_identical(result[["prediction_draws_used"]], c(2L, 2L))
  testthat::expect_true(all(
    result[["prediction_range"]] == "group_observed"
  ))
  testthat::expect_true(all(result[["source_model_file"]] == "model_a.qs"))
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
