testthat::test_that(
  "build_horizontal_hvarpart_density_strip() rotates the first strip",
  {
    plot_input <-
      ggplot2::ggplot(
        data.frame(
          x = 1:2,
          y = 1:2,
          panel = factor(c("Density", "POL"))
        ),
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
      ) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(
        cols = ggplot2::vars(.data[["panel"]]),
        switch = "x"
      ) +
      ggplot2::theme(
        strip.text.x.bottom = ggplot2::element_text(angle = 90)
      )
    result <-
      plot_input |>
      ggplot2::ggplotGrob() |>
      build_horizontal_hvarpart_density_strip()
    strip_position <-
      which(result[["layout"]][["name"]] == "strip-b-1")
    strip_tree <-
      result[["grobs"]][[strip_position]][["grobs"]][[1]]
    title_position <-
      which(
        grepl(
          "^strip.text.x.bottom",
          names(strip_tree[["children"]])
        )
      )
    text_grob <-
      strip_tree[["children"]][[title_position]][["children"]][[1]]

    testthat::expect_s3_class(result, "gtable")
    testthat::expect_identical(text_grob[["rot"]], 0)
  }
)

testthat::test_that(
  "build_horizontal_hvarpart_density_strip() validates input",
  {
    testthat::expect_error(
      build_horizontal_hvarpart_density_strip(list()),
      "must be supplied as a gtable"
    )
  }
)
