testthat::test_that("validate_analysis_profiles() accepts valid profiles", {
  data_profiles <-
    tibble::tibble(
      profile_id = "main_spd",
      analysis_family = "h1",
      analytical_unit = "within_dataset",
      human_proxy = "spd",
      human_predictor_specification = "spd",
      spd_radius_specification = "250_km_with_500_km_fallback",
      structural_control = "time",
      profile_role = "canonical",
      enabled = TRUE,
      seed = 1234L,
      configuration_reference = "main"
    )

  testthat::expect_silent(
    validate_analysis_profiles(data_profiles = data_profiles)
  )
})

testthat::test_that("validate_analysis_profiles() rejects duplicate IDs", {
  data_profiles <-
    tibble::tibble(
      profile_id = c("main", "main"),
      analysis_family = "h1",
      analytical_unit = "within_dataset",
      human_proxy = "spd",
      human_predictor_specification = "spd",
      spd_radius_specification = "250_km",
      structural_control = "time",
      profile_role = "canonical",
      enabled = TRUE,
      seed = 1234L,
      configuration_reference = "main"
    )

  testthat::expect_error(
    validate_analysis_profiles(data_profiles = data_profiles),
    regexp = "unique"
  )
})

testthat::test_that(
  "validate_analysis_profiles() accepts an SPD-radius sensitivity",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("main_spd", "spd_radius_100"),
        analysis_family = "h1",
        analytical_unit = "within_dataset",
        human_proxy = "spd",
        human_predictor_specification = "spd",
        spd_radius_specification = c("250_km", "100_km"),
        structural_control = "time",
        profile_role = c("canonical", "sensitivity"),
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = c("main", "spd_radius")
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() rejects unsupported combinations",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = "bad_spatial_unit",
        analysis_family = "h1",
        analytical_unit = "within_dataset",
        human_proxy = "events",
        human_predictor_specification = "all_event_groups",
        spd_radius_specification = "not_applicable",
        structural_control = "space",
        profile_role = "sensitivity",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = "invalid"
      )

    testthat::expect_error(
      validate_analysis_profiles(data_profiles = data_profiles),
      regexp = "unsupported combination"
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() protects canonical operations",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("main_spd", "alternate_spd"),
        analysis_family = "h1",
        analytical_unit = "within_dataset",
        human_proxy = "spd",
        human_predictor_specification = "spd",
        spd_radius_specification = c("250_km", "100_km"),
        structural_control = "time",
        profile_role = "canonical",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = c("main", "alternate")
      )

    testthat::expect_error(
      validate_analysis_profiles(data_profiles = data_profiles),
      regexp = "Only one canonical profile"
    )
  }
)
