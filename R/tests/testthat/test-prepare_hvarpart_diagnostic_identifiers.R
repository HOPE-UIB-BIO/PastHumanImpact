testthat::test_that("prepare_hvarpart_diagnostic_identifiers() prepends IDs", {
  result <- prepare_hvarpart_diagnostic_identifiers(
    data.frame(value = 1:2), data.frame(dataset_id = "a")
  )
  testthat::expect_identical(dplyr::pull(result, dataset_id), c("a", "a"))
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_error(
    prepare_hvarpart_diagnostic_identifiers(
      data.frame(value = 1), data.frame(dataset_id = c("a", "b"))
    ),
    "contract"
  )
})
