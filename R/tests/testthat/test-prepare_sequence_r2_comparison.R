testthat::test_that("prepare_sequence_r2_comparison() returns paired metrics", {
  balance <- tibble::tibble(
    dataset_id = "a", total_adjusted_r_squared = 0.2,
    signed_balance = -0.1, region = "Europe", climatezone = "Polar",
    long = 1, lat = 2
  )
  components <- tibble::tibble(
    dataset_id = "a", measure = "unique_adjusted_r2",
    component = c("human", "climate", "time"), value = c(0.1, 0.2, 0.05)
  )
  unique_r2 <- tibble::tibble(
    dataset_id = "a",
    fraction = c("pure_human", "pure_climate", "pure_time"),
    adjusted_r_squared = c(0.2, 0.1, 0.05)
  )
  result <- prepare_sequence_r2_comparison(
    balance, components,
    dplyr::mutate(balance, total_adjusted_r_squared = 0.3),
    unique_r2
  )
  testthat::expect_equal(nrow(result[["comparison_long"]]), 4L)
  testthat::expect_equal(
    dplyr::pull(result[["sequence_values"]], delta_total), 0.1
  )
  testthat::expect_error(
    prepare_sequence_r2_comparison(list(), components, balance, unique_r2),
    "data frames"
  )
})
