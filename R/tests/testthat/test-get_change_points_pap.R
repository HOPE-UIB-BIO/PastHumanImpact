testthat::test_that("get_change_points_pap() validates required columns", {
  bad_source <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_change_points_pap(data_source = bad_source),
    regexp = "missing required columns"
  )
})

testthat::test_that("get_change_points_pap() validates nested table columns", {
  bad_source <-
    data.frame(
      dataset_id = 1,
      mvrt_cp = I(list(numeric(0))),
      PAP_diversity = I(list(1)),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      PAP_roc = I(list(data.frame(Age = 100, ROC = 1, Peak = FALSE))),
      dcca_scores = I(list(data.frame(sample_id = "s1", axis_1 = 0.1))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_change_points_pap(data_source = bad_source),
    regexp = "PAP_diversity"
  )
})
