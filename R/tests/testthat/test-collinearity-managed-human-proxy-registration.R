testthat::test_that("filtered joint and bridge profiles are registered", {
  profiles <- readr::read_csv(
    here::here("R", "analyses", "00_profiles", "analysis_profiles.csv"),
    show_col_types = FALSE
  )
  selected <- profiles |>
    dplyr::filter(
      .data[["configuration_reference"]] ==
        "human_proxy_hvarpart_collinearity_managed"
    )
  testthat::expect_equal(nrow(selected), 4L)
  testthat::expect_equal(
    as.integer(table(selected$analytical_unit)[c(
      "time_slice", "within_dataset"
    )]),
    c(2L, 2L)
  )
  testthat::expect_setequal(
    selected$human_predictor_specification,
    c(
      "filtered_sqrt_spd_kk10_sqrt_hyde",
      "sqrt_spd_matched_filtered_climate"
    )
  )
  testthat::expect_invisible(validate_analysis_profiles(profiles))

  contracts <- readr::read_csv(
    here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
    show_col_types = FALSE
  )
  contract <- contracts |>
    dplyr::filter(
      .data[["pipeline_id"]] ==
        "human_proxy_hvarpart_collinearity_managed"
    )
  testthat::expect_equal(nrow(contract), 1L)
  testthat::expect_true(file.exists(here::here(contract$script[[1]])))
  testthat::expect_true(file.exists(here::here(contract$runner[[1]])))
  testthat::expect_match(
    contract$public_targets, "files_colmanaged_hvarpart_figures"
  )
  testthat::expect_match(
    contract$public_targets, "files_colmanaged_decision_variant_figures"
  )
  testthat::expect_match(
    contract$public_targets,
    "files_colmanaged_decision_temporal_comparison"
  )
  testthat::expect_match(
    contract$public_targets, "output_colmanaged_proxy_spatial_atlas"
  )
  testthat::expect_match(
    contract$public_targets, "files_colmanaged_proxy_spatial_atlas"
  )
  testthat::expect_match(
    contract$public_targets,
    "files_colmanaged_proxy_spatial_atlas_tables"
  )
})

testthat::test_that("semantic primary figure filenames are locked", {
  pipeline <- readLines(
    here::here(
      "R", "analyses", "91_sensitivity_analyses",
      "human_proxy_hvarpart_collinearity_managed", "pipeline.R"
    ),
    warn = FALSE
  )
  text <- paste(pipeline, collapse = "\n")
  testthat::expect_match(
    text, "collinearity_filtered_joint_human_proxies__human_climate_balance__"
  )
  testthat::expect_match(
    text, "collinearity_filtered_joint_human_proxies__human_climate_space__"
  )
  testthat::expect_match(
    text, "predictor_selection__continental_region_and_region"
  )
  testthat::expect_match(text, "fig3_canonical_spd_common")
  testthat::expect_match(text, "fig3_matched_spd_bridge_common")
  testthat::expect_match(text, "fig3_filtered_joint_common")
  testthat::expect_match(text, "fig4_three_model_comparison_common")
  testthat::expect_match(text, "Proxy_spatial_atlas")
  testthat::expect_false(grepl(
    "predictor_selection__continental_region_and_climate_zone",
    text,
    fixed = TRUE
  ))
})
