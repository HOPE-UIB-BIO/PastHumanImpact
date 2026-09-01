testthat::test_that(
  "run_spd_radius_h1_profile() preserves its delegation contract",
  {
    received <- NULL
    test_environment <- new.env(parent = globalenv())
    test_environment[["run_h1_control_profile"]] <- function(...) {
      received <<- list(...)
      list(marker = "delegated")
    }
    sys.source(
      here::here(
        "R/functions/hvarpart/sensitivity/run_spd_radius_h1_profile.R"
      ),
      envir = test_environment
    )
    predictor_data <- tibble::tibble(
      dataset_id = "a",
      radius_km = 250,
      spd_radius_specification = "250_km",
      data_merge = list(tibble::tibble())
    )
    properties <- tibble::tibble(dataset_id = "a")
    metadata <- tibble::tibble(dataset_id = "a")
    predictors <- list(human = "spd", climate = "temp_annual")
    config <- list(seed = 1234L)
    profiles <- tibble::tibble(profile_id = "profile")

    result <- test_environment[["run_spd_radius_h1_profile"]](
      data_predictors_profile = predictor_data,
      data_properties_filtered = properties,
      data_meta = metadata,
      response_vars = "n0",
      predictor_vars = predictors,
      analysis_config = config,
      data_profiles = profiles
    )

    testthat::expect_identical(result[["marker"]], "delegated")
    testthat::expect_identical(
      received[["data_predictors_profile"]],
      predictor_data
    )
    testthat::expect_identical(received[["predictor_vars"]], predictors)
    testthat::expect_identical(received[["data_profiles"]], profiles)
  }
)
