testthat::test_that("select_local_hvarpart_predictors() selects both blocks", {
  data <- tibble::tibble(h = 1:10, c = c(1:5, 7:11))
  result <- select_local_hvarpart_predictors(
    data_source = data,
    human_candidates = "h",
    climate_candidates = "c"
  )
  testthat::expect_identical(
    result[["predictor_vars"]],
    list(human = "h", climate = "c")
  )
  testthat::expect_equal(nrow(result[["selection_errors"]]), 0L)
  testthat::expect_error(
    select_local_hvarpart_predictors(data, "missing", "c"),
    "contract"
  )
})
