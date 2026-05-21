testthat::test_that("merge_indicators_and_indices() merges to nested events", {
  data_source_indices <-
    data.frame(
      dataset_id = c(1, 1),
      age = c(10, 20),
      weak_indicies = c(FALSE, TRUE),
      strong_indicies = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    )

  data_source_indicators <-
    data.frame(
      dataset_id = c(1, 1),
      age = c(10, 20),
      weak_indicators = c(TRUE, FALSE),
      strong_indicators = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    )

  result <-
    merge_indicators_and_indices(
      data_source_indices = data_source_indices,
      data_source_indicators = data_source_indicators
    )

  expected_data <-
    data.frame(
      age = c(10, 20),
      no_impact = c(0, 0),
      weak = c(1, 1),
      strong = c(1, 1)
    )

  vec_dataset_id <-
    result[["dataset_id"]]

  data_events_updated <-
    purrr::pluck(result, "events_updated", 1)

  testthat::expect_identical(
    vec_dataset_id,
    1
  )

  testthat::expect_identical(
    data_events_updated,
    expected_data
  )
})

testthat::test_that("merge_indicators_and_indices() returns one row per dataset", {
  data_source_indices <-
    data.frame(
      dataset_id = c(1, 2),
      age = c(100, 100),
      weak_indicies = c(FALSE, TRUE),
      strong_indicies = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    )

  data_source_indicators <-
    data.frame(
      dataset_id = c(1, 2),
      age = c(100, 100),
      weak_indicators = c(TRUE, FALSE),
      strong_indicators = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    )

  result <-
    merge_indicators_and_indices(
      data_source_indices = data_source_indices,
      data_source_indicators = data_source_indicators
    )

  testthat::expect_identical(dplyr::pull(result, dataset_id), c(1, 2))
  testthat::expect_equal(nrow(result), 2L)
})

testthat::test_that("merge_indicators_and_indices() sets no_impact when both weak and strong are false", {
  data_source_indices <-
    data.frame(
      dataset_id = 10,
      age = c(100, 200),
      weak_indicies = c(FALSE, FALSE),
      strong_indicies = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    )

  data_source_indicators <-
    data.frame(
      dataset_id = 10,
      age = c(100, 200),
      weak_indicators = c(FALSE, FALSE),
      strong_indicators = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    )

  result <-
    merge_indicators_and_indices(
      data_source_indices = data_source_indices,
      data_source_indicators = data_source_indicators
    )

  data_events <- dplyr::pull(result, events_updated)[[1]]
  testthat::expect_identical(dplyr::pull(data_events, no_impact), c(1, 1))
  testthat::expect_identical(dplyr::pull(data_events, weak), c(0, 0))
  testthat::expect_identical(dplyr::pull(data_events, strong), c(0, 0))
})