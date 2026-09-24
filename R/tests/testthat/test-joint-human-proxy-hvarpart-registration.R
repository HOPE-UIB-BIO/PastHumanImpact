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
