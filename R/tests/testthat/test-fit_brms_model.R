testthat::test_that("fit_brms_model() validates integer max_chains", {
  data_source <-
    data.frame(
      dataset_id = c("d1", "d1", "d2"),
      stratum = c("s1", "s1", "s1"),
      age_ka = c(0, 1, 0),
      value = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    fit_brms_model(
      data_source = data_source,
      max_chains = 1.5,
      verbose = FALSE
    ),
    regexp = "positive integers"
  )
})

testthat::test_that("fit_brms_model() validates integer sel_k", {
  data_source <-
    data.frame(
      dataset_id = c("d1", "d1", "d2"),
      stratum = c("s1", "s1", "s1"),
      age_ka = c(0, 1, 0),
      value = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    fit_brms_model(
      data_source = data_source,
      sel_k = 2.2,
      verbose = FALSE
    ),
    regexp = "positive integers"
  )
})

testthat::test_that("fit_brms_model() returns NA when model fitting fails", {
  data_source <-
    data.frame(
      dataset_id = c("d1", "d1", "d2"),
      stratum = c("s1", "s1", "s1"),
      age_ka = c(0, 1, 0),
      value = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  result <-
    fit_brms_model(
      data_source = data_source,
      family_key = "not_a_valid_family",
      max_chains = 1,
      total_iterations = 100,
      min_iterations_per_chain = 100,
      verbose = FALSE
    )

  testthat::expect_identical(result, NA_real_)
})

testthat::test_that("fit_brms_model() accepts a config row", {
  data_source <-
    data.frame(
      model_id = "pap_temporal__n0",
      analysis = "pap_temporal",
      variable = "n0",
      dataset_id = c("d1", "d1", "d2"),
      stratum = c("s1", "s1", "s1"),
      age_ka = c(0, 1, 0),
      value = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  model_config_row <-
    data.frame(
      model_id = "pap_temporal__n0",
      variable = "n0",
      family_key = "not_a_valid_family",
      model_profile = "stratum_fs",
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      stratum_var = "stratum",
      total_iterations = 100,
      min_iterations_per_chain = 100,
      max_chains = 1,
      stringsAsFactors = FALSE
    )

  result <-
    fit_brms_model(
      data_source = data_source,
      model_config_row = model_config_row,
      verbose = FALSE
    )

  testthat::expect_identical(result, NA_real_)
})
