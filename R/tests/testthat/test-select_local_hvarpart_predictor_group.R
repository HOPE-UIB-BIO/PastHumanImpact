testthat::test_that("select_local_hvarpart_predictor_group() keeps preference", {
  set.seed(900723)
  data <- tibble::tibble(
    preferred = seq_len(50),
    redundant = .data[["preferred"]] + stats::rnorm(50, sd = 0.001)
  )
  result <- select_local_hvarpart_predictor_group(
    data_source = data,
    candidates = c("preferred", "redundant"),
    preference = c("preferred", "redundant"),
    group_name = "human"
  )
  testthat::expect_identical(result[["selected"]], "preferred")
  testthat::expect_identical(
    result[["status"]], "eligible_for_design_check"
  )
  testthat::expect_error(
    select_local_hvarpart_predictor_group(
      data, "missing", "missing", "human"
    ),
    "contract"
  )
})
