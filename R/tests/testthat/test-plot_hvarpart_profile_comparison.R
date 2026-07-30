testthat::test_that("profile plot accepts signed values outside zero to one", {
  data_profiles <-
    tidyr::expand_grid(
      aggregation_level = c("analysis", "model"),
      model_id = c("model_a", "model_b"),
      profile = c("signed", "zero_truncated", "exclude_negative"),
      predictor = c("human", "climate")
    ) |>
    dplyr::mutate(
      analysis = "spatial_spd",
      pooled_allocation = dplyr::case_when(
        .data[["profile"]] == "signed" &
          .data[["predictor"]] == "human" ~ -0.2,
        .data[["profile"]] == "signed" ~ 1.2,
        .data[["predictor"]] == "human" ~ 0.1,
        .default = 0.9
      ),
      n_models = 2L
    )

  result <- plot_hvarpart_profile_comparison(data_profiles)

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(range(result$data$pooled_allocation), c(-0.2, 1.2))
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomViolin"),
    logical(1)
  )))
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) {
      inherits(layer$geom, "GeomHline") &&
        identical(layer$aes_params$linetype, 2)
    },
    logical(1)
  )))
  testthat::expect_equal(
    result$coordinates$limits$y,
    c(-0.5, 1.5)
  )
})

testthat::test_that("profile plot validates required columns", {
  testthat::expect_error(
    plot_hvarpart_profile_comparison(tibble::tibble()),
    "columns are missing"
  )
})
