testthat::test_that("joint human-proxy sensitivity is registered", {
  profiles <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "analysis_profiles.csv"),
      show_col_types = FALSE
    )
  contracts <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
      show_col_types = FALSE
    )

  testthat::expect_true(
    all(
      c(
        "within_dataset_joint_human_proxies_time_control",
        "time_slice_joint_human_proxies_spatial_control"
      ) %in% profiles[["profile_id"]]
    )
  )
  testthat::expect_true(
    "joint_human_proxy_hvarpart" %in% contracts[["pipeline_id"]]
  )
})

testthat::test_that("SPD plus KK10 sensitivity is registered", {
  profiles <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "analysis_profiles.csv"),
      show_col_types = FALSE
    )
  contracts <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
      show_col_types = FALSE
    )

  testthat::expect_true(
    base::all(
      c(
        "within_dataset_spd_kk10_time_control",
        "time_slice_spd_kk10_spatial_control"
      ) %in% profiles[["profile_id"]]
    )
  )
  testthat::expect_true(
    "spd_kk10_hvarpart" %in% contracts[["pipeline_id"]]
  )
})
