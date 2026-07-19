testthat::test_that("prepare_predictor_model_data() prepares long model data", {
  data_predictors <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      data_merge = list(
        tibble::tibble(
          age = c(0, 500),
          temp_annual = c(1, 2)
        ),
        tibble::tibble(
          age = c(0, 500),
          temp_annual = c(3, 4)
        )
      )
    )

  data_meta <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = c("Europe", "Europe"),
      data_publicity = c("public", "public")
    )

  data_regions <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = c("Europe", "Europe")
    )

  data_climatezones <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      climatezone = c("Temperate", "Temperate")
    )

  result <-
    prepare_predictor_model_data(
      data_predictors = data_predictors,
      data_meta = data_meta,
      data_regions = data_regions,
      data_climatezones = data_climatezones,
      age_from = 0,
      age_to = 500,
      min_records = 2
    )

  data_model <- result[["data_model"]]

  testthat::expect_true(is.list(result))
  testthat::expect_s3_class(data_model, "data.frame")
  testthat::expect_setequal(unique(data_model[["analysis"]]), "predictor_temporal")
  testthat::expect_setequal(unique(data_model[["variable"]]), "temp_annual")
  testthat::expect_true(all(data_model[["n_records"]] == 2))
})

testthat::test_that("prepare_predictor_model_data() returns constants separately", {
  data_predictors <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      data_merge = list(
        tibble::tibble(age = c(0, 500), temp_annual = c(1, 1)),
        tibble::tibble(age = c(0, 500), temp_annual = c(1, 1))
      )
    )

  data_meta <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = c("Europe", "Europe"),
      data_publicity = c("public", "public")
    )

  data_regions <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = c("Europe", "Europe")
    )

  data_climatezones <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      climatezone = c("Temperate", "Temperate")
    )

  result <-
    prepare_predictor_model_data(
      data_predictors = data_predictors,
      data_meta = data_meta,
      data_regions = data_regions,
      data_climatezones = data_climatezones,
      age_from = 0,
      age_to = 500,
      min_records = 2
    )

  testthat::expect_true(nrow(result[["data_model"]]) > 0)
  testthat::expect_true(nrow(result[["data_constant"]]) > 0)
})
