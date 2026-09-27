testthat::test_that("local spatial summaries retain model identifiers", {
  designs <- tibble::tibble(
    region = "Europe", age = 2000, model_id = "joint",
    selection_status = "missing_human_predictor",
    data_merge = list(tibble::tibble(dataset_id = "a")),
    predictor_vars = list(list(human = character(), climate = "c")),
    selection_audit = list(tibble::tibble())
  )
  fitted <- fit_local_spatial_hvarpart_models(designs, "response")
  result <- summarise_local_spatial_hvarpart_models(fitted)
  testthat::expect_true("status" %in% names(result))
  testthat::expect_identical(
    dplyr::pull(result[["status"]], model_id), "joint"
  )
  testthat::expect_error(
    summarise_local_spatial_hvarpart_models(data.frame(x = 1)),
    "contract"
  )
})
