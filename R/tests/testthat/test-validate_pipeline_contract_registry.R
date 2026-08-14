testthat::test_that(
  "pipeline contracts require unique ownership and public targets",
  {
    script <- tempfile(fileext = ".R")

    writeLines("list()", script)

    contracts <-
      tibble::tibble(
        pipeline_id = "pipeline",
        script = script,
        store_relative_path = "analysis/pipeline",
        public_targets = "result",
        runner = script
      )

    testthat::expect_silent(
      validate_pipeline_contract_registry(contracts)
    )

    duplicated <- dplyr::bind_rows(contracts, contracts)

    testthat::expect_error(
      validate_pipeline_contract_registry(duplicated),
      regexp = "invalid"
    )
  }
)
