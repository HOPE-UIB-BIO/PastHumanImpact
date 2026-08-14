testthat::test_that("untruncated colours clamp endpoint values", {
  result <-
    resolve_untruncated_importance_colour(
      values = c(-1, 0, 0.5, 1, 2),
      palette = c("white", "red")
    )

  testthat::expect_identical(result[[1]], result[[2]])

  testthat::expect_identical(result[[4]], result[[5]])

  testthat::expect_length(result, 5L)
})
