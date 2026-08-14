testthat::test_that("build_evidence_manifest() records available files", {
  path_artifact <-
    tempfile(fileext = ".csv")

  readr::write_lines("value", path_artifact)

  data_artifacts <-
    tibble::tibble(
      artifact_id = "artifact_a",
      description = "Test artifact",
      analysis_profile = "main",
      source_pipeline = "example/pipeline.R",
      public_target = "file_example",
      path = path_artifact
    )

  result <-
    build_evidence_manifest(data_artifacts = data_artifacts)

  testthat::expect_true(result[["file_exists"]])
  testthat::expect_false(is.na(result[["file_hash"]]))
  testthat::expect_identical(
    result[["validation_status"]],
    "available"
  )
})
