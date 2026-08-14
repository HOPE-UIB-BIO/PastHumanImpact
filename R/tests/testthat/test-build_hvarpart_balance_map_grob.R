testthat::test_that(
  "build_hvarpart_balance_map_grob() adds a regional map",
  {
    base_plot <- ggplot2::ggplot() + ggplot2::geom_blank()
    base_grob <- ggplot2::ggplotGrob(base_plot)
    result <-
      build_hvarpart_balance_map_grob(
        current_grob = base_grob,
        map_plot = base_plot,
        panel_row = 1L
      )

    testthat::expect_s3_class(result, "gtable")
    testthat::expect_gt(length(result[["grobs"]]), length(base_grob[["grobs"]]))
    testthat::expect_equal(
      sum(result[["layout"]][["name"]] == "map-frame"),
      1L
    )
  }
)
