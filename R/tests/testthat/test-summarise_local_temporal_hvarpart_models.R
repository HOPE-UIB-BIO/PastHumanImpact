testthat::test_that("local temporal summaries retain model identifiers", {
  designs <- tibble::tibble(
    dataset_id = "a", model_id = "joint",
    selection_status = "missing_human_predictor",
    data_merge = list(tibble::tibble(age = 2000)),
    predictor_vars = list(list(human = character(), climate = "c")),
    selection_audit = list(tibble::tibble())
  )
  fitted <- fit_local_temporal_hvarpart_models(designs, "response")
  result <- summarise_local_temporal_hvarpart_models(fitted)
  testthat::expect_named(
    result, c("status", "components", "unique_adjusted_r2", "residual_moran")
  )
  testthat::expect_identical(
    dplyr::pull(result[["status"]], model_id), "joint"
  )
  testthat::expect_error(
    summarise_local_temporal_hvarpart_models(data.frame(x = 1)),
    "contract"
  )
})
