testthat::test_that("atlas map draws world and Europe panels", {
  values <- tibble::tibble(
    dataset_id = c("a", "b"), proxy_label = "sqrt(SPD)",
    colour_value = c(0.2, 0.4), colour_max = 1,
    long = c(10, 20), lat = c(50, 60), region = c("Europe", "Asia")
  )
  world <- tibble::tibble(
    long = c(-180, 180, 180, -180), lat = c(-60, -60, 85, 85), group = 1L
  )
  result <- plot_human_proxy_spatial_atlas_map(
    values, world, view = "europe"
  )
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(length(result[["layers"]]), 2L)
  testthat::expect_error(
    plot_human_proxy_spatial_atlas_map(data.frame(x = 1), world),
    "contract"
  )
})
