testthat::test_that("get_model_newdata() creates stratum age grid", {
  data_source <-
    tibble::tibble(
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold"),
      stratum = c("Europe__Temperate", "Asia__Cold"),
      dataset_id = c("d1", "d2"),
      age = c(0, 0),
      age_ka = c(0, 0),
      variable = c("n0", "n0"),
      value = c(1, 2)
    )

  model_config_row <-
    tibble::tibble(
      variable = "n0",
      x_var = "age_ka",
      group_var = "dataset_id",
      stratum_var = "stratum",
      age_min = 0,
      age_max = 1000,
      timestep = 500
    )

  result <-
    get_model_newdata(
      data_source = data_source,
      model_config_row = model_config_row
    )

  testthat::expect_identical(nrow(result), 6L)
  testthat::expect_setequal(result[["age"]], c(0, 500, 1000))
  testthat::expect_true(all(c("region", "climatezone", "stratum") %in% names(result)))
})

testthat::test_that("get_model_newdata() filters by config stratum", {
  data_source <-
    tibble::tibble(
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold"),
      stratum = c("Europe__Temperate", "Asia__Cold"),
      dataset_id = c("d1", "d2"),
      age = c(0, 0),
      age_ka = c(0, 0),
      variable = c("n0", "n0"),
      value = c(1, 2)
    )

  model_config_row <-
    tibble::tibble(
      variable = "n0",
      region = "Europe",
      climatezone = "Temperate",
      x_var = "age_ka",
      group_var = "dataset_id",
      stratum_var = "stratum",
      age_min = 0,
      age_max = 1000,
      timestep = 500
    )

  result <-
    get_model_newdata(
      data_source = data_source,
      model_config_row = model_config_row
    )

  testthat::expect_identical(nrow(result), 3L)
  testthat::expect_identical(unique(result[["region"]]), "Europe")
  testthat::expect_identical(unique(result[["climatezone"]]), "Temperate")
})
