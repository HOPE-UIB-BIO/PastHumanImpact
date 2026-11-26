testthat::test_that("single-proxy H1 profiles cover every run once", {
  data_profiles <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "analysis_profiles.csv"),
      show_col_types = FALSE
    )

  data_selected <-
    data_profiles |>
    dplyr::filter(.data[["configuration_reference"]] == "human_proxy_h1")

  testthat::expect_equal(base::nrow(data_selected), 28L)
  testthat::expect_equal(
    base::sort(base::unique(dplyr::pull(data_selected, "analytical_unit"))),
    c("time_slice", "within_dataset")
  )

  data_runs <-
    data_selected |>
    dplyr::count(
      .data[["human_proxy"]],
      .data[["proxy_support"]],
      .data[["age_window"]]
    )

  testthat::expect_equal(base::nrow(data_runs), 14L)
  testthat::expect_true(base::all(dplyr::pull(data_runs, "n") == 2L))

  data_main <-
    data_selected |>
    dplyr::filter(.data[["profile_role"]] == "canonical")

  testthat::expect_equal(base::nrow(data_main), 12L)
  testthat::expect_setequal(
    dplyr::pull(data_main, "human_proxy"),
    c("spd", "kk10", "hyde_3_5_land_use")
  )
  testthat::expect_true(
    base::all(dplyr::pull(data_main, "proxy_support") == "spd_buffer")
  )
  testthat::expect_invisible(validate_analysis_profiles(data_profiles))
})

testthat::test_that("single-proxy H1 pipelines are registered", {
  data_profiles <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "analysis_profiles.csv"),
      show_col_types = FALSE
    ) |>
    dplyr::filter(.data[["configuration_reference"]] == "human_proxy_h1")

  data_contracts <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
      show_col_types = FALSE
    )

  data_runs <-
    data_contracts |>
    dplyr::filter(
      .data[["pipeline_id"]] %in% dplyr::pull(data_profiles, "profile_id")
    )

  testthat::expect_equal(base::nrow(data_runs), 28L)
  testthat::expect_equal(
    dplyr::pull(data_runs, "store_relative_path"),
    stringr::str_c("analyses_h1/", dplyr::pull(data_runs, "pipeline_id"))
  )
  testthat::expect_equal(
    dplyr::pull(data_runs, "script_profile"),
    dplyr::pull(data_runs, "pipeline_id")
  )
  testthat::expect_true(
    base::all(base::file.exists(here::here(dplyr::pull(data_runs, "script"))))
  )
  testthat::expect_true(
    base::all(
      c("h1_human_proxy_inputs", "h1_human_proxy_outcomes") %in%
        dplyr::pull(data_contracts, "pipeline_id")
    )
  )

  data_upstream <-
    data_contracts |>
    dplyr::filter(.data[["pipeline_id"]] %in% c("paps", "predictors"))

  testthat::expect_match(
    dplyr::pull(data_upstream, "public_targets"),
    "(^|;)data_(properties|predictors)(;|$)"
  )
  testthat::expect_invisible(
    withr::with_dir(
      new = here::here(),
      code = validate_pipeline_contract_registry(data_contracts)
    )
  )
})
