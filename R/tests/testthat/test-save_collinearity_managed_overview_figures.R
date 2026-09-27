testthat::test_that("save_collinearity_managed_overview_figures() writes files", {
  temp_path <- tempfile("colmanaged-overview-")
  dir.create(temp_path)
  plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  paths <- save_collinearity_managed_overview_figures(
    plot_r2 = plot,
    plot_selection = plot,
    path_r2 = file.path(temp_path, "r2"),
    path_selection = file.path(temp_path, "selection")
  )
  testthat::expect_length(paths, 4L)
  testthat::expect_true(all(file.exists(paths)))
})

testthat::test_that("save_collinearity_managed_overview_figures() validates", {
  testthat::expect_error(
    save_collinearity_managed_overview_figures(
      plot_r2 = data.frame(), plot_selection = data.frame(),
      path_r2 = "a", path_selection = "b"
    ),
    "contract"
  )
})
