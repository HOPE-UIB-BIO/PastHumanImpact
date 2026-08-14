testthat::test_that(
  "report rendering validates its source path",
  {
    testthat::expect_error(
      render_reporting_document(
        input = tempfile(fileext = ".qmd")
      ),
      regexp = "invalid"
    )
  }
)
