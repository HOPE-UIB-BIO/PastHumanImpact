testthat::test_that("load_analysis_profiles() loads a valid CSV", {
  path_profiles <-
    tempfile(fileext = ".csv")

  data_profiles <-
    tibble::tibble(
      profile_id = "main_events",
      analysis_family = "h1",
      analytical_unit = "time_slice",
      human_proxy = "events",
      human_predictor_specification = "all_events",
      spd_radius_specification = "not_applicable",
      structural_control = "space",
      profile_role = "canonical",
      enabled = TRUE,
      seed = 1234L,
      configuration_reference = "main"
    )

  readr::write_csv(data_profiles, path_profiles)

  result <-
    load_analysis_profiles(path = path_profiles)

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(result[["profile_id"]], "main_events")
})

testthat::test_that("load_analysis_profiles() rejects missing files", {
  testthat::expect_error(
    load_analysis_profiles(path = tempfile()),
    regexp = "existing"
  )
})
