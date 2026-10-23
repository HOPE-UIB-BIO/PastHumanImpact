testthat::test_that(
  "save_human_proxy_spatial_atlas() writes age PNGs and a multi-page PDF",
  {
    output_directory <- tempfile(pattern = "proxy-spatial-atlas-")
    plots <- list(
      `8000` = ggplot2::ggplot() + ggplot2::theme_void(),
      `7500` = ggplot2::ggplot() + ggplot2::theme_void()
    )

    paths <- save_human_proxy_spatial_atlas(
      plot_pages = plots,
      output_directory = output_directory,
      basename = "test_atlas",
      width_mm = 100,
      height_mm = 70,
      dpi = 72
    )

    testthat::expect_length(paths, 3L)
    testthat::expect_true(all(file.exists(paths)))
    testthat::expect_true(any(grepl("test_atlas[.]pdf$", paths)))
    testthat::expect_true(any(grepl("08000_cal_bp[.]png$", paths)))
    testthat::expect_true(any(grepl("07500_cal_bp[.]png$", paths)))
    testthat::expect_identical(
      eval(formals(save_human_proxy_spatial_atlas)[["basename"]]),
      "matched_human_proxies__spatial_atlas__world_and_europe__2_to_8ka"
    )
  }
)
