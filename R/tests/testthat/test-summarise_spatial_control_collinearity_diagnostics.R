testthat::test_that("spatial control diagnostics skip ineligible models", {
  data <- tibble::tibble(
    region = "Europe", age = 2000, model_id = "joint",
    data_merge = list(tibble::tibble(dataset_id = "a")),
    predictor_vars = list(list(human = character(), climate = "c")),
    result = list(list(status = "missing_human_predictor"))
  )
  result <- summarise_spatial_control_collinearity_diagnostics(data, "response")
  testthat::expect_named(
    result, c("correlations", "vif", "condition_indices", "design")
  )
  testthat::expect_true(all(purrr::map_int(result, nrow) == 0L))
})
