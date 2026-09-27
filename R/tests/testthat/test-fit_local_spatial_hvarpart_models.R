testthat::test_that("local spatial wrapper preserves classified exclusions", {
  designs <- tibble::tibble(
    region = "Europe", age = 2000, model_id = "joint",
    selection_status = "missing_human_predictor",
    data_merge = list(tibble::tibble(dataset_id = "a")),
    predictor_vars = list(list(human = character(), climate = "c")),
    selection_audit = list(tibble::tibble())
  )
  result <- fit_local_spatial_hvarpart_models(designs, "response")
  testthat::expect_identical(
    result[["result"]][[1]][["status"]], "missing_human_predictor"
  )
  testthat::expect_true(is.na(dplyr::pull(result, error_message)))
  testthat::expect_error(
    fit_local_spatial_hvarpart_models(data.frame(x = 1), "response"),
    "contract"
  )
})
