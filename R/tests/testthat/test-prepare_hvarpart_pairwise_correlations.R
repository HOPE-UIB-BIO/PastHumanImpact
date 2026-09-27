testthat::test_that("prepare_hvarpart_pairwise_correlations() labels blocks", {
  data <- tibble::tibble(h = 1:6, c = 6:1)
  result <- prepare_hvarpart_pairwise_correlations(
    data, c("h", "c"), list(human = "h", climate = "c"),
    character(), "selected"
  )
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_identical(dplyr::pull(result, group_1), "human")
  testthat::expect_true(dplyr::pull(result, high_correlation))
  testthat::expect_error(
    prepare_hvarpart_pairwise_correlations(
      data, "missing", list(human = "h", climate = "c"),
      character(), "selected"
    ),
    "contract"
  )
})
