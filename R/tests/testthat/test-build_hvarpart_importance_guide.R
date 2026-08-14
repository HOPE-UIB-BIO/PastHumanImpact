testthat::test_that("importance guide restores a labelled vertical axis", {
  result <- build_hvarpart_importance_guide(
    ggplot2::ggplot(),
    "Signed allocation"
  )

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_identical(result$labels$y, "Signed allocation")
  testthat::expect_s3_class(result$theme$axis.line.y, "element_line")
})

testthat::test_that("importance guide validates inputs", {
  testthat::expect_error(
    build_hvarpart_importance_guide(list(), character()),
    "must be a ggplot"
  )
})
