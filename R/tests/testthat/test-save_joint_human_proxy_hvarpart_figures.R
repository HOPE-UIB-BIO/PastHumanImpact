testthat::test_that("joint HVarPart figure exporter writes four files", {
  plot <-
    ggplot2::ggplot(
      tibble::tibble(x = 1, y = 1),
      ggplot2::aes(.data[["x"]], .data[["y"]])
    ) +
    ggplot2::geom_point()
  output <- file.path(tempdir(), "joint-hvarpart-figures")

  result <-
    save_joint_human_proxy_hvarpart_figures(
      plot_spatial = plot,
      plot_temporal = plot,
      path_spatial = file.path(output, "spatial"),
      path_temporal = file.path(output, "temporal")
    )

  testthat::expect_length(result, 4L)
  testthat::expect_true(all(file.exists(result)))
})
