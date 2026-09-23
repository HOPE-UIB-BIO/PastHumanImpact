testthat::test_that("save_human_proxy_convergence_figures() writes formats", {
  plot <-
    ggplot2::ggplot(
      tibble::tibble(x = 1, y = 1),
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
    ) +
    ggplot2::geom_point()

  path_dir <-
    file.path(tempdir(), "human-proxy-figures")

  result <-
    save_human_proxy_convergence_figures(
      plot_overall = plot,
      plot_regions = plot,
      path_overall = file.path(path_dir, "overall"),
      path_regions = file.path(path_dir, "regions")
    )

  testthat::expect_length(result, 4L)
  testthat::expect_true(all(file.exists(result)))
})
