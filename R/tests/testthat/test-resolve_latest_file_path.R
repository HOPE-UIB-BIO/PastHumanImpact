testthat::test_that(
  "resolve_latest_file_path() returns a complete existing path",
  {
    data_directory <- withr::local_tempdir()
    data_path <- file.path(data_directory, "example_data_2026-08-14.rds")

    saveRDS(list(value = 1), data_path)

    result <-
      resolve_latest_file_path(
        file_name = "example_data",
        dir = data_directory
      )

    testthat::expect_true(file.exists(result))
    testthat::expect_equal(
      normalizePath(result, winslash = "/"),
      normalizePath(data_path, winslash = "/")
    )
  }
)

testthat::test_that(
  "resolve_latest_file_path() rejects a missing file family",
  {
    data_directory <- withr::local_tempdir()

    testthat::expect_error(
      resolve_latest_file_path(
        file_name = "missing_data",
        dir = data_directory
      ),
      "No versioned file"
    )
  }
)
