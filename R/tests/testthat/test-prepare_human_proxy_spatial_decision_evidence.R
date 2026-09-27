testthat::test_that("spatial decision evidence uses a common cohort", {
  balance <- tibble::tibble(
    dataset_id = "a", total_adjusted_r_squared = 0.4,
    human = 0.2, climate = 0.1, time = 0.1,
    signed_difference = 0.1, signed_balance = 0.25,
    signed_weight = 0.4, zero_balance = 1 / 3, zero_weight = 0.3,
    region = "Europe", climatezone = "Cold", long = 10, lat = 50
  )
  components <- tibble::tibble(
    dataset_id = "a", component = c("human", "climate", "time"),
    measure = "unique_adjusted_r2", value = 0.1
  )
  unique_r2 <- tidyr::crossing(
    model_id = c("spd_matched_bridge", "joint_filtered"),
    dataset_id = "a",
    fraction = c("pure_human", "pure_climate", "pure_time")
  ) |>
    dplyr::mutate(adjusted_r_squared = 0.1)
  result <- prepare_human_proxy_spatial_decision_evidence(
    balance, components, balance, balance, unique_r2
  )
  testthat::expect_equal(nrow(result[["spatial_common_cohort"]]), 3L)
  testthat::expect_equal(
    dplyr::pull(result[["spatial_summary"]], n_sequences),
    rep(1L, 3L)
  )
  testthat::expect_error(
    prepare_human_proxy_spatial_decision_evidence(
      list(), components, balance, balance, unique_r2
    ),
    "data frames"
  )
})
