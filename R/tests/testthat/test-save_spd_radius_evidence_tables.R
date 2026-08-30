testthat::test_that("save_spd_radius_evidence_tables() writes named CSVs", {
  path_output <-
    file.path(tempdir(), "spd-radius-evidence", "table.csv")

  result <-
    save_spd_radius_evidence_tables(
      data_tables = list(source = tibble::tibble(value = 1)),
      file_paths = c(source = path_output)
    )

  testthat::expect_true(file.exists(result))
  testthat::expect_identical(names(result), "source")
  testthat::expect_identical(
    readr::read_csv(result, show_col_types = FALSE)[["value"]],
    1
  )
})
