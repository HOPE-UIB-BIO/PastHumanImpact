testthat::test_that("prepare_human_proxy_model_contrasts() computes differences", {
  filtered_balance <- tibble::tibble(
    model_id = "joint_filtered", dataset_id = "a", zero_balance = 0.5
  )
  unfiltered_balance <- tibble::tibble(dataset_id = "a", zero_balance = 0.2)
  filtered_composition <- tibble::tibble(
    model_id = "joint_filtered", region = "Europe", age = 2000,
    predictor = "human", allocation = 0.6
  )
  unfiltered_composition <- tibble::tibble(
    region = "Europe", age = 2000, predictor = "human", allocation = 0.4
  )
  result <- prepare_human_proxy_model_contrasts(
    filtered_balance, filtered_composition,
    unfiltered_balance, unfiltered_composition
  )
  testthat::expect_equal(
    dplyr::pull(result[["filtered_unfiltered_joint_balance"]],
                filtered_minus_unfiltered),
    0.3
  )
  testthat::expect_error(
    prepare_human_proxy_model_contrasts(list(), data.frame()),
    "contract"
  )
})
