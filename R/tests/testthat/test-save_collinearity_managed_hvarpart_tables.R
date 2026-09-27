testthat::test_that("save_collinearity_managed_hvarpart_tables() writes CSV", {
  path <- tempfile(fileext = ".csv")
  tables <- list(a = tibble::tibble(x = 1L, nested = list(c("a", "b"))))
  result <- save_collinearity_managed_hvarpart_tables(
    tables, c(a = path)
  )
  testthat::expect_true(file.exists(result[[1]]))
  written <- readr::read_csv(result[[1]], show_col_types = FALSE)
  testthat::expect_identical(dplyr::pull(written, nested), "a;b")
  testthat::expect_error(
    save_collinearity_managed_hvarpart_tables(tables, c(b = path)),
    "contract"
  )
})
