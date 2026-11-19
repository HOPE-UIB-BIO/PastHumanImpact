testthat::test_that("design-diagnostic profiles match the model variants", {
  profiles <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "analysis_profiles.csv"),
      show_col_types = FALSE
    )

  selected <-
    profiles |>
    dplyr::filter(
      .data[["configuration_reference"]] ==
        "human_proxy_hvarpart_design_diagnostics"
    )

  specs <-
    build_human_proxy_diagnostic_specifications()

  vec_spec_profiles <-
    c(
      dplyr::pull(specs, "within_dataset_profile_id"),
      stats::na.omit(dplyr::pull(specs, "time_slice_profile_id"))
    )

  testthat::expect_equal(nrow(selected), 9L)
  testthat::expect_setequal(
    dplyr::pull(selected, "profile_id"),
    vec_spec_profiles
  )
  testthat::expect_true(all(dplyr::pull(selected, "profile_role") ==
    "sensitivity"))
  testthat::expect_invisible(validate_analysis_profiles(profiles))
})

testthat::test_that("design-diagnostic pipeline contract is registered", {
  contracts <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
      show_col_types = FALSE
    )

  contract <-
    contracts |>
    dplyr::filter(
      .data[["pipeline_id"]] == "human_proxy_hvarpart_design_diagnostics"
    )

  testthat::expect_equal(nrow(contract), 1L)
  testthat::expect_true(
    file.exists(here::here(dplyr::pull(contract, "script")))
  )
  testthat::expect_true(
    file.exists(here::here(dplyr::pull(contract, "runner")))
  )
  testthat::expect_match(
    dplyr::pull(contract, "public_targets"),
    "files_designdiag_tables"
  )
  testthat::expect_match(
    dplyr::pull(contract, "store_relative_path"),
    "^sensitivity_analyses/"
  )
})
