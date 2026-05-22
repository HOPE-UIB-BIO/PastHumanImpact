testthat::test_that("merge_all_events() row-binds multiple event tables", {
  data_a <-
    data.frame(
      dataset_id = 1,
      age = 100,
      weak = TRUE,
      strong = FALSE
    )

  data_b <-
    data.frame(
      dataset_id = 2,
      age = 200,
      weak = FALSE,
      strong = TRUE
    )

  result <-
    merge_all_events(data_a, data_b)

  testthat::expect_identical(
    as.data.frame(result),
    data.frame(
      dataset_id = c(1, 2),
      age = c(100, 200),
      weak = c(TRUE, FALSE),
      strong = c(FALSE, TRUE)
    )
  )
})

testthat::test_that("merge_all_events() keeps all rows from all inputs", {
  data_a <- data.frame(dataset_id = 1, age = 100, weak = TRUE, strong = FALSE)
  data_b <- data.frame(dataset_id = 2, age = 200, weak = FALSE, strong = TRUE)
  data_c <- data.frame(dataset_id = 3, age = 300, weak = TRUE, strong = TRUE)

  result <- merge_all_events(data_a, data_b, data_c)

  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_identical(dplyr::pull(result, dataset_id), c(1, 2, 3))
})

testthat::test_that("merge_all_events() handles empty inputs", {
  data_a <- data.frame(dataset_id = integer(), age = numeric(), weak = logical(), strong = logical())
  data_b <- data.frame(dataset_id = 5, age = 500, weak = FALSE, strong = FALSE)

  result <- merge_all_events(data_a, data_b)

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_identical(dplyr::pull(result, dataset_id), 5)
})

testthat::test_that("merge_all_events() validates input types", {
  data_a <-
    data.frame(dataset_id = 1, age = 100, weak = TRUE, strong = FALSE)

  testthat::expect_error(
    merge_all_events(data_a, "not_a_table"),
    regexp = "must be data frames"
  )
})