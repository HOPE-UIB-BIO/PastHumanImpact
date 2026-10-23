testthat::test_that("joint HVarPart table exporter preserves names", {
  output <- file.path(tempdir(), "joint-hvarpart-tables", "status.csv")

  result <-
    save_joint_human_proxy_hvarpart_tables(
      data_tables = list(status = tibble::tibble(value = 1)),
      file_paths = c(status = output)
    )

  testthat::expect_identical(names(result), "status")
  testthat::expect_true(file.exists(result))
})
