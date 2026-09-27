testthat::test_that("temporal comparison delegates to canonical layout", {
  specs <- tibble::tibble(model_id = "joint_filtered")
  data <- tidyr::crossing(
    analysis = "joint", region = "Europe", age = c(2000, 2500),
    predictor = c("human", "climate", "space")
  ) |>
    dplyr::mutate(allocation = 1 / 3)
  result <- plot_human_proxy_temporal_comparison(data, specs)
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_error(
    plot_human_proxy_temporal_comparison(
      data, tibble::tibble(model_id = "wrong")
    ),
    "contract"
  )
})
