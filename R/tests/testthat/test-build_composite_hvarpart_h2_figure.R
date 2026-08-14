testthat::test_that("H2 composite includes a complete labelled grid", {
  empty_plot <- ggplot2::ggplot()
  result <-
    build_composite_hvarpart_h2_figure(
      plot_list = rep(list(empty_plot), 4L),
      region_labels = c("Region A", "Region B"),
      climatezone_labels = c("Zone A", "Zone B"),
      climatezone_colours = c("#8C4418", "#562FB1"),
      importance_guide = empty_plot,
      importance_title = "Importance",
      predictor_legend = empty_plot,
      age_legend = empty_plot,
      trajectory_guide = empty_plot
    )

  testthat::expect_s3_class(result, "ggplot")
})

testthat::test_that("H2 composite rejects incomplete grids", {
  empty_plot <- ggplot2::ggplot()

  testthat::expect_error(
    build_composite_hvarpart_h2_figure(
      plot_list = list(empty_plot),
      region_labels = c("Region A", "Region B"),
      climatezone_labels = c("Zone A", "Zone B"),
      climatezone_colours = c("#8C4418", "#562FB1"),
      importance_guide = empty_plot,
      importance_title = "Importance",
      predictor_legend = empty_plot,
      age_legend = empty_plot,
      trajectory_guide = empty_plot
    ),
    "complete grid"
  )
})

testthat::test_that("H2 composite requires one header colour per column", {
  empty_plot <- ggplot2::ggplot()

  testthat::expect_error(
    build_composite_hvarpart_h2_figure(
      plot_list = rep(list(empty_plot), 4L),
      region_labels = c("Region A", "Region B"),
      climatezone_labels = c("Zone A", "Zone B"),
      climatezone_colours = "#8C4418",
      importance_guide = empty_plot,
      importance_title = "Importance",
      predictor_legend = empty_plot,
      age_legend = empty_plot,
      trajectory_guide = empty_plot
    ),
    "complete grid"
  )
})
