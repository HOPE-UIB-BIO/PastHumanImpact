testthat::test_that("plot_hvarpart_importance() plots both predictors", {
  data_importance <-
    tibble::tibble(
      dataset_id = c("core_a", "core_a", "core_b", "core_b"),
      predictor = rep(c("human", "climate"), 2),
      importance_percent = c(60, 40, 20, 80),
      total_explained_variation = c(0.35, 0.35, 0.2, 0.2)
    )

  res_plot <-
    plot_hvarpart_importance(
      data_importance = data_importance,
      dataset_id = "core_a",
      predictor_palette = c(
        human = "#D2A62C",
        climate = "#2A7F7F"
      )
    )

  testthat::expect_s3_class(res_plot, "ggplot")
  testthat::expect_length(res_plot[["layers"]], 3L)
  testthat::expect_identical(
    unique(res_plot[["data"]][["dataset_id"]]),
    "core_a"
  )
  testthat::expect_match(
    res_plot[["labels"]][["subtitle"]],
    "35.0%",
    fixed = TRUE
  )
})

testthat::test_that("plot_hvarpart_importance() requires two predictors", {
  data_importance <-
    tibble::tibble(
      dataset_id = "core_a",
      predictor = "human",
      importance_percent = 100,
      total_explained_variation = 0.35
    )

  testthat::expect_error(
    plot_hvarpart_importance(
      data_importance = data_importance,
      dataset_id = "core_a",
      predictor_palette = c(
        human = "#D2A62C",
        climate = "#2A7F7F"
      )
    ),
    "human and climate"
  )
})
