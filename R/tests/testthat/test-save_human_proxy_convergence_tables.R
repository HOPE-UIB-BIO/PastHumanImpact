testthat::test_that("save_human_proxy_convergence_tables() writes tables", {
  path_output <-
    file.path(tempdir(), "human-proxy-evidence", "table.csv.gz")

  result <-
    save_human_proxy_convergence_tables(
      data_tables = list(values = tibble::tibble(value = 1)),
      file_paths = c(values = path_output)
    )

  testthat::expect_true(file.exists(result))
  testthat::expect_identical(names(result), "values")
})
