testthat::test_that("build_empty_temporal_hvarpart_result() matches contract", {
  result <- build_empty_temporal_hvarpart_result("rank_deficient", 10L)
  testthat::expect_identical(result[["status"]], "rank_deficient")
  testthat::expect_identical(result[["n_samples"]], 10L)
  testthat::expect_s3_class(result[["unique_adjusted_r2"]], "data.frame")
  testthat::expect_error(
    build_empty_temporal_hvarpart_result(c("a", "b"), 1L),
    "contract"
  )
})
