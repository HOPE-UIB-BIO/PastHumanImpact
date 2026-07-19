testthat::test_that("plot_event_temporal_trends() builds the plot", {
  data_predictions <-
    tibble::tibble(
      region = factor(rep("Europe", 4)),
      climatezone = factor(
        rep("Temperate_Without_dry_season", 4)
      ),
      age = c(500, 1000, 500, 1000),
      variable = c("bi", "bi", "fi", "fi"),
      estimate = c(0.8, 0.7, 0.2, 0.3),
      conf_low = c(0.7, 0.6, 0.1, 0.2),
      conf_high = c(0.9, 0.8, 0.3, 0.4)
    )

  res_plot <-
    plot_event_temporal_trends(
      data_predictions = data_predictions
    )

  testthat::expect_s3_class(res_plot, "ggplot")
  testthat::expect_length(res_plot[["layers"]], 4L)
  testthat::expect_equal(
    res_plot[["layers"]][[4]][["data"]][["xintercept"]],
    2
  )
  scale_age <-
    res_plot[["scales"]][["scales"]][[1]]
  testthat::expect_equal(
    scale_age[["trans"]][["inverse"]](scale_age[["limits"]]),
    c(8.5, 0)
  )
  testthat::expect_equal(
    levels(res_plot[["data"]][["event_label"]])[1:2],
    c("no impact", "first impact")
  )
})

testthat::test_that("plot_event_temporal_trends() validates inputs", {
  data_predictions <-
    tibble::tibble(variable = "bi")

  testthat::expect_error(
    plot_event_temporal_trends(
      data_predictions = data_predictions
    ),
    "missing required columns"
  )

  data_unknown_event <-
    tibble::tibble(
      region = "Europe",
      climatezone = "Temperate_Without_dry_season",
      age = 500,
      variable = "unknown",
      estimate = 0.5,
      conf_low = 0.4,
      conf_high = 0.6
    )

  testthat::expect_error(
    plot_event_temporal_trends(
      data_predictions = data_unknown_event
    ),
    "recognised event codes"
  )
})
