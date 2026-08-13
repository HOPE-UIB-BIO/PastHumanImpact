testthat::test_that(
  "build_hvarpart_density_divider() adds one divider per row",
  {
    plot_input <-
      ggplot2::ggplot(
        data.frame(
          x = 1:4,
          y = 1:4,
          row = rep(c("one", "two"), each = 2),
          column = rep(c("Density", "POL"), 2)
        ),
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
      ) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["row"]]),
        cols = ggplot2::vars(.data[["column"]])
      )
    result <-
      plot_input |>
      ggplot2::ggplotGrob() |>
      build_hvarpart_density_divider()

    testthat::expect_s3_class(result, "gtable")
    testthat::expect_equal(
      sum(result[["layout"]][["name"]] == "density-divider"),
      2
    )
  }
)

testthat::test_that(
  "build_hvarpart_density_divider() validates input",
  {
    testthat::expect_error(
      build_hvarpart_density_divider(list()),
      "do not satisfy"
    )
  }
)
