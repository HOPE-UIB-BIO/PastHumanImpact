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

testthat::test_that(
  "validate_analysis_profiles() accepts human-event sensitivity profiles",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("combined", "event_aggregation"),
        analysis_family = "h1",
        analytical_unit = c("within_dataset", "spatial_aggregation"),
        human_proxy = c("spd_events", "events"),
        human_predictor_specification = c(
          "spd_plus_region_events",
          "region_specific_events"
        ),
        spd_radius_specification = c(
          "250_km_with_500_km_fallback",
          "not_applicable"
        ),
        structural_control = c("time", "time_and_space"),
        profile_role = "sensitivity",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = "human_event_inclusion_as_coded"
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() accepts joint human-proxy sensitivities",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("joint_time", "joint_space"),
        analysis_family = "h1",
        analytical_unit = c("within_dataset", "time_slice"),
        human_proxy = "spd_kk10_hyde",
        human_predictor_specification = "sqrt_spd_kk10_sqrt_hyde",
        spd_radius_specification = "250_km_with_500_km_fallback",
        structural_control = c("time", "space"),
        profile_role = "sensitivity",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = "joint_human_proxy_8ka_2ka"
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )

    testthat::expect_error(
      validate_analysis_profiles(
        dplyr::mutate(
          data_profiles,
          spd_radius_specification = "not_applicable"
        )
      ),
      regexp = "unsupported combination"
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() accepts SPD plus KK10 sensitivities",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("spd_kk10_time", "spd_kk10_space"),
        analysis_family = "h1",
        analytical_unit = c("within_dataset", "time_slice"),
        human_proxy = "spd_kk10",
        human_predictor_specification = "sqrt_spd_kk10_fraction",
        spd_radius_specification = "250_km_with_500_km_fallback",
        structural_control = c("time", "space"),
        profile_role = "sensitivity",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = "spd_kk10_8ka_2ka"
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )

    testthat::expect_error(
      validate_analysis_profiles(
        dplyr::mutate(
          data_profiles,
          spd_radius_specification = "not_applicable"
        )
      ),
      regexp = "unsupported combination"
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() accepts HYDE 3.5 single-proxy profiles",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("hyde_land_use_time", "hyde_population_space"),
        analysis_family = "h1",
        analytical_unit = c("within_dataset", "time_slice"),
        human_proxy = c("hyde_3_5_land_use", "hyde_3_5_population"),
        human_predictor_specification = c(
          "hyde_3_5_land_use_fraction",
          "sqrt_hyde_3_5_population_density"
        ),
        spd_radius_specification = "250_km_with_500_km_fallback",
        structural_control = c("time", "space"),
        profile_role = "sensitivity",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = "human_proxy_h1",
        age_window = c("common_cohort", "native"),
        proxy_support = "spd_buffer"
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() separates main profiles by window",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = c("spd_reference", "spd_common", "spd_native"),
        analysis_family = "h1",
        analytical_unit = "within_dataset",
        human_proxy = "spd",
        human_predictor_specification = "sqrt_spd",
        spd_radius_specification = "250_km_with_500_km_fallback",
        structural_control = "time",
        profile_role = "canonical",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = c("main", "human_proxy_h1", "human_proxy_h1"),
        age_window = c("not_applicable", "common_cohort", "native"),
        proxy_support = c("not_applicable", "spd_buffer", "spd_buffer")
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )

    testthat::expect_error(
      validate_analysis_profiles(
        dplyr::mutate(data_profiles, age_window = "native")
      ),
      regexp = "Only one canonical profile"
    )
  }
)

testthat::test_that(
  "validate_analysis_profiles() rejects unknown windows and supports",
  {
    data_profiles <-
      tibble::tibble(
        profile_id = "kk10_common",
        analysis_family = "h1",
        analytical_unit = "within_dataset",
        human_proxy = "kk10",
        human_predictor_specification = "kk10_land_use_fraction",
        spd_radius_specification = "not_applicable",
        structural_control = "time",
        profile_role = "sensitivity",
        enabled = TRUE,
        seed = 1234L,
        configuration_reference = "human_proxy_h1",
        age_window = "common_cohort",
        proxy_support = "radius_50km"
      )

    testthat::expect_silent(
      validate_analysis_profiles(data_profiles = data_profiles)
    )

    testthat::expect_error(
      validate_analysis_profiles(
        dplyr::mutate(data_profiles, age_window = "early")
      ),
      regexp = "window and support"
    )

    testthat::expect_error(
      validate_analysis_profiles(
        dplyr::mutate(data_profiles, proxy_support = "radius_75km")
      ),
      regexp = "window and support"
    )

    testthat::expect_error(
      validate_analysis_profiles(
        dplyr::mutate(data_profiles, age_window = NA_character_)
      ),
      regexp = "window and support"
    )
  }
)
