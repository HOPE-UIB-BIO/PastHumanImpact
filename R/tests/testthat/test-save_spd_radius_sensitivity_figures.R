testthat::test_that("save_spd_radius_sensitivity_figures() writes formats", {
  plot_object <-
    ggplot2::ggplot(
      tibble::tibble(x = 1, y = 1),
      ggplot2::aes(.data[["x"]], .data[["y"]])
    ) +
    ggplot2::geom_point()
  path_dir <- file.path(tempdir(), "spd-radius-figures")

  result <-
    save_spd_radius_sensitivity_figures(
      plot_spatial = plot_object,
      plot_temporal_profiles = plot_object,
      plot_temporal_changes = plot_object,
      path_spatial = file.path(path_dir, "spatial"),
      path_temporal_profiles = file.path(path_dir, "temporal-profiles"),
      path_temporal_changes = file.path(path_dir, "temporal-changes")
    )

  testthat::expect_length(result, 6L)
  testthat::expect_true(all(file.exists(result)))
  testthat::expect_setequal(
    tools::file_ext(result),
    c("png", "pdf")
  )
})
