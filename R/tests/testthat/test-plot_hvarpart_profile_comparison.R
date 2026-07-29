testthat::test_that("profile plot accepts signed values outside zero to one", {
  data_profiles <- tibble::tibble(
    analysis = rep("spatial_spd", 6L),
    predictor = rep(c("human", "climate"), 3L),
    profile = rep(
      c("signed", "zero_truncated", "exclude_negative"),
      each = 2L
    ),
    pooled_allocation = c(-0.2, 1.2, 0, 1, 0.1, 0.9),
    n_models = rep(c(10L, 10L, 8L), each = 2L)
  )

  result <- plot_hvarpart_profile_comparison(data_profiles)

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(range(result$data$pooled_allocation), c(-0.2, 1.2))
})

testthat::test_that("profile plot validates required columns", {
  testthat::expect_error(
    plot_hvarpart_profile_comparison(tibble::tibble()),
    "columns are missing"
  )
})
