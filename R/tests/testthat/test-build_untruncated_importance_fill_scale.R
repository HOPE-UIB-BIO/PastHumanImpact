testthat::test_that("untruncated fill scale has a fixed domain", {
  result <-
    build_untruncated_importance_fill_scale(
      palette = c("white", "pink", "red")
    )

  testthat::expect_s3_class(result, "ScaleContinuous")

  testthat::expect_equal(result[["limits"]], c(0, 1))
})

testthat::test_that("untruncated fill scale validates its palette", {
  testthat::expect_error(
    build_untruncated_importance_fill_scale(palette = "red"),
    "requires three colours"
  )
})
