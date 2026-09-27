testthat::test_that("save_unfiltered_joint_reference() preserves hierarchy", {
  source <- tempfile("unfiltered-source-")
  destination <- tempfile("unfiltered-destination-")
  dir.create(source)
  source_file <- file.path(source, "evidence.txt")
  writeLines("evidence", source_file)
  result <- save_unfiltered_joint_reference(source, destination)
  testthat::expect_length(result, 1L)
  testthat::expect_true(file.exists(result[[1]]))
  testthat::expect_identical(readLines(result[[1]]), "evidence")
  testthat::expect_error(
    save_unfiltered_joint_reference(source, character()),
    "contract"
  )
})
