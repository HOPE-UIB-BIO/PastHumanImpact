testthat::test_that("empty authorizations never invoke fitting", {
  path_config <-
    tempfile()

  path_models <-
    tempfile()

  dir.create(path_config)

  dir.create(path_models)

  result <-
    run_authorized_temporal_models(
      data_authorized = tibble::tibble(
        model_id = character(),
        request_id = character()
      ),
      data_source = tibble::tibble(),
      config_dir = path_config,
      model_dir = path_models,
      path_history = tempfile(),
      verbose = FALSE
    )

  testthat::expect_equal(nrow(result), 0L)
})
