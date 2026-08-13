testthat::test_that("prepare_model_prediction_data() creates stratum age grid", {
  data_source <-
    tibble::tibble(
      region = c("Europe", "Europe", "Asia"),
      climatezone = c("Temperate", "Temperate", "Cold"),
      stratum = c(
        "Europe__Temperate",
        "Europe__Temperate",
        "Asia__Cold"
      ),
      dataset_id = factor(
        c("d1", "d2", "d3"),
        levels = c("d1", "d2", "d3", "unused")
      ),
      age = c(0, 0, 0),
      age_ka = c(0, 0, 0),
      variable = c("n0", "n0", "n0"),
      value = c(1, 2, 3)
    )

  model_config_row <-
    tibble::tibble(
      variable = "n0",
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 0.5,
      x_sd = 0.5,
      group_var = "dataset_id",
      stratum_var = "stratum",
      age_min = 0,
      age_max = 1000,
      timestep = 500
    )

  result <-
    prepare_model_prediction_data(
      data_source = data_source,
      model_config_row = model_config_row
    )

  testthat::expect_identical(nrow(result), 9L)
  testthat::expect_setequal(result[["age"]], c(0, 500, 1000))
  testthat::expect_true(all(
    c("region", "climatezone", "stratum") %in% names(result)
  ))
  testthat::expect_setequal(result[["age_ka_scaled"]], c(-1, 0, 1))
  testthat::expect_setequal(result[["dataset_id"]], c("d1", "d2", "d3"))
  testthat::expect_false("unused" %in% levels(result[["dataset_id"]]))
  testthat::expect_equal(
    result[["age_ka_scaled"]] * 0.5 + 0.5,
    result[["age_ka"]]
  )
})

testthat::test_that("prepare_model_prediction_data() filters by config stratum", {
  data_source <-
    tibble::tibble(
      region = c("Europe", "Europe", "Asia"),
      climatezone = c("Temperate", "Temperate", "Cold"),
      stratum = c(
        "Europe__Temperate",
        "Europe__Temperate",
        "Asia__Cold"
      ),
      dataset_id = c("d1", "d2", "d3"),
      age = c(0, 0, 0),
      age_ka = c(0, 0, 0),
      variable = c("n0", "n0", "n0"),
      value = c(1, 2, 3)
    )

  model_config_row <-
    tibble::tibble(
      variable = "n0",
      region = "Europe",
      climatezone = "Temperate",
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 0.5,
      x_sd = 0.5,
      group_var = "dataset_id",
      stratum_var = "stratum",
      age_min = 0,
      age_max = 1000,
      timestep = 500
    )

  result <-
    prepare_model_prediction_data(
      data_source = data_source,
      model_config_row = model_config_row
    )

  testthat::expect_identical(nrow(result), 6L)
  testthat::expect_identical(unique(result[["region"]]), "Europe")
  testthat::expect_identical(unique(result[["climatezone"]]), "Temperate")
  testthat::expect_setequal(result[["dataset_id"]], c("d1", "d2"))
  testthat::expect_setequal(levels(result[["dataset_id"]]), c("d1", "d2"))
})

testthat::test_that("prepare_model_prediction_data() respects dataset age ranges", {
  data_source <-
    tibble::tibble(
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      dataset_id = c("d1", "d1", "d2", "d2"),
      age = c(2000, 2500, 2500, 3000),
      age_ka = c(2, 2.5, 2.5, 3),
      variable = "spd",
      value = c(1, 2, 3, 4)
    )
  model_config_row <-
    tibble::tibble(
      variable = "spd",
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 0.5,
      x_sd = 0.5,
      group_var = "dataset_id",
      stratum_var = "stratum",
      age_min = 0,
      age_max = 3000,
      timestep = 500,
      region = "Europe",
      climatezone = "Temperate"
    )

  result <-
    prepare_model_prediction_data(
      data_source = data_source,
      model_config_row = model_config_row,
      prediction_range = "group_observed"
    )

  testthat::expect_identical(nrow(result), 4L)
  testthat::expect_equal(
    result %>%
      dplyr::group_by(dataset_id) %>%
      dplyr::summarise(
        age_min = min(age),
        age_max = max(age),
        .groups = "drop"
      ) %>%
      dplyr::pull(age_min),
    c(2000, 2500)
  )
  testthat::expect_equal(
    result %>%
      dplyr::group_by(dataset_id) %>%
      dplyr::summarise(
        age_max = max(age),
        .groups = "drop"
      ) %>%
      dplyr::pull(age_max),
    c(2500, 3000)
  )
})

testthat::test_that("prepare_model_prediction_data() never extrapolates SPD below 2 ka", {
  data_source <-
    tibble::tibble(
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      dataset_id = "d1",
      age = c(1500, 2000, 2500),
      age_ka = c(1.5, 2, 2.5),
      variable = "spd",
      value = c(1, 2, 3)
    )
  model_config_row <-
    tibble::tibble(
      variable = "spd",
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      x_mean = 2,
      x_sd = 0.5,
      group_var = "dataset_id",
      stratum_var = "stratum",
      age_min = 0,
      age_max = 3000,
      timestep = 500
    )

  result <- prepare_model_prediction_data(data_source, model_config_row)

  testthat::expect_gte(min(result[["age"]]), 2000)
})
