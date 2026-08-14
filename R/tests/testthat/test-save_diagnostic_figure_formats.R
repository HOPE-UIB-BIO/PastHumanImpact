testthat::test_that(
  "diagnostic figures are saved in both standard formats",
  {
    directory <- tempfile()
    dir.create(directory)

    paths <-
      save_diagnostic_figure_formats(
        fig_object = ggplot2::ggplot(),
        fig_name = "diagnostic",
        path_figures = directory,
        width = 20,
        height = 20,
        units = "mm"
      )

    testthat::expect_true(all(file.exists(paths)))
    testthat::expect_setequal(tools::file_ext(paths), c("png", "pdf"))
  }
)
