testthat::test_that("plot grob is added to the selected row", {
  layout_plot <-
    ggplot2::ggplot(
      tibble::tibble(x = 1, y = 1),
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
    ) +
    ggplot2::geom_point()

  layout_grob <-
    ggplot2::ggplotGrob(layout_plot)

  panel_row <-
    layout_grob[["layout"]] |>
    dplyr::filter(.data[["name"]] == "panel") |>
    dplyr::pull(.data[["t"]])

  result <-
    build_plot_grob_row(
      current_grob = layout_grob,
      map_plot = layout_plot,
      panel_row = panel_row
    )

  testthat::expect_s3_class(result, "gtable")

  testthat::expect_gt(
    length(result[["grobs"]]),
    length(layout_grob[["grobs"]])
  )
})
