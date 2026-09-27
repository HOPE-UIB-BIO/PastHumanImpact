testthat::test_that("diagnose_local_hvarpart_collinearity() compares stages", {
  set.seed(900723)
  data <- tibble::tibble(
    h = stats::rnorm(30), c = stats::rnorm(30), time = stats::rnorm(30)
  )
  result <- diagnose_local_hvarpart_collinearity(
    data_source = data,
    predictor_vars = list(human = "h", climate = "c"),
    candidate_vars = c("h", "c"),
    control_vars = "time"
  )
  testthat::expect_named(
    result, c("correlations", "vif", "condition_indices", "design")
  )
  testthat::expect_setequal(
    unique(dplyr::pull(result[["design"]], stage)),
    c("before_selection", "after_selection_with_controls")
  )
  testthat::expect_error(
    diagnose_local_hvarpart_collinearity(data, list(), character()),
    "contract"
  )
})
