testthat::test_that("get_all_predicted_general_trends returns empty for empty predictors input", {
  data_source <-
    data.frame(
      region = character(0),
      climatezone = character(0),
      variable = character(0),
      stringsAsFactors = FALSE
    )

  result <-
    get_all_predicted_general_trends(
      data_source = data_source,
      sel_type = "predictors"
    )

  testthat::expect_identical(nrow(result), 0L)
})

testthat::test_that("get_all_predicted_general_trends returns empty for empty events input", {
  data_source <-
    data.frame(
      region = character(0),
      climatezone = character(0),
      variable = character(0),
      stringsAsFactors = FALSE
    )

  result <-
    get_all_predicted_general_trends(
      data_source = data_source,
      sel_type = "events"
    )

  testthat::expect_identical(nrow(result), 0L)
})

testthat::test_that("get_all_predicted_general_trends validates required columns", {
  bad_source <-
    data.frame(
      region = character(0),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_all_predicted_general_trends(
      data_source = bad_source,
      sel_type = "events"
    ),
    regexp = "climatezone"
  )
})