testthat::test_that("resolve_pipeline_store_path() builds a stable path", {
  result <-
    resolve_pipeline_store_path(
      data_storage_path = "D:/project-data",
      store_relative_path = "analyses_h1/inputs"
    )

  testthat::expect_match(
    result,
    "Targets_data[\\\\/]analyses_h1[\\\\/]inputs$"
  )
})

testthat::test_that("resolve_pipeline_store_path() rejects traversal", {
  testthat::expect_error(
    resolve_pipeline_store_path(
      data_storage_path = "D:/project-data",
      store_relative_path = "../outside"
    ),
    regexp = "traverse"
  )
})
