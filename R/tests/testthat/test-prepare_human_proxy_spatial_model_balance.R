testthat::test_that("prepare_human_proxy_spatial_model_balance() standardises", {
  data <- tibble::tibble(
    dataset_id = "a", model_id = "source_model",
    total_adjusted_r_squared = 0.3,
    human = 0.2, climate = 0.1, time = 0.05,
    signed_difference = 0.1, signed_balance = 1 / 3,
    signed_weight = 0.3, zero_balance = 1 / 3, zero_weight = 0.3
  )
  result <- prepare_human_proxy_spatial_model_balance(
    data, tibble::tibble(dataset_id = "a"), "joint", "Joint"
  )
  testthat::expect_identical(dplyr::pull(result, model_id), "joint")
  testthat::expect_equal(dplyr::pull(result, human_contribution), 0.2)
  testthat::expect_error(
    prepare_human_proxy_spatial_model_balance(
      data, data.frame(x = "a"), "joint", "Joint"
    ),
    "contract"
  )
})
