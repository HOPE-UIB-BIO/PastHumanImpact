testthat::test_that("joint temporal plot uses one normalized stack", {
  data <-
    tidyr::crossing(
      analysis = "temporal_joint_human_proxies",
      region = c("Europe", "Asia"),
      age = c(2000, 2500),
      predictor = c("human", "climate", "space")
    ) |>
    dplyr::mutate(
      allocation = dplyr::recode(
        .data[["predictor"]],
        human = 0.5,
        climate = 0.3,
        space = 0.2
      )
    )

  plot <- plot_h1_temporal_joint_human_proxy_composition(data)
  built <- ggplot2::ggplot_build(plot)

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_true(all(plot[["data"]][["x_position"]] == 1))
  testthat::expect_equal(nrow(plot[["data"]]), nrow(data))
  testthat::expect_true(length(built[["data"]]) >= 4L)
})

testthat::test_that("joint temporal plot rejects non-normalized stacks", {
  data <-
    tibble::tibble(
      analysis = "temporal_joint_human_proxies",
      region = "Europe",
      age = 2000,
      predictor = c("human", "climate", "space"),
      allocation = c(0.5, 0.4, 0.2)
    )

  testthat::expect_error(
    plot_h1_temporal_joint_human_proxy_composition(data),
    regexp = "sum exactly to one"
  )
})
