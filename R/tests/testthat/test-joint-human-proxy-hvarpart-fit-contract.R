testthat::test_that("time-controlled HVarPart accepts three human predictors", {
  set.seed(342)
  n <- 13L
  data <-
    tibble::tibble(
      age = seq(2000, 8000, by = 500),
      response_1 = stats::rnorm(n),
      response_2 = stats::rnorm(n),
      human_1 = stats::rnorm(n),
      human_2 = stats::rnorm(n),
      human_3 = stats::rnorm(n),
      climate_1 = stats::rnorm(n),
      climate_2 = stats::rnorm(n),
      climate_3 = stats::rnorm(n),
      climate_4 = stats::rnorm(n)
    )
  predictors <-
    list(
      human = c("human_1", "human_2", "human_3"),
      climate = c("climate_1", "climate_2", "climate_3", "climate_4")
    )

  result <-
    fit_temporal_hvarpart_dataset(
      data_dataset = data,
      response_vars = c("response_1", "response_2"),
      predictor_vars = predictors,
      min_unique_ages = 10L,
      min_residual_df = 4L,
      distance_years = 500,
      permutations = 9L,
      seed = 342L
    )

  testthat::expect_true(
    result[["status"]] %in%
      c("estimated", "estimated_residual_temporal_dependence")
  )
  testthat::expect_identical(result[["residual_df"]], 4L)
  testthat::expect_true(
    all(c("pure_human", "pure_climate", "pure_time") %in%
      result[["unique_adjusted_r2"]][["fraction"]])
  )
})

testthat::test_that("spatially controlled HVarPart accepts three human predictors", {
  set.seed(343)
  n <- 40L
  data <-
    tibble::tibble(
      dataset_id = as.character(seq_len(n)),
      long = rep(seq(-10, 10, length.out = 8), each = 5),
      lat = rep(seq(40, 60, length.out = 5), times = 8),
      response_1 = stats::rnorm(n),
      response_2 = stats::rnorm(n),
      human_1 = stats::rnorm(n),
      human_2 = stats::rnorm(n),
      human_3 = stats::rnorm(n),
      climate_1 = stats::rnorm(n),
      climate_2 = stats::rnorm(n),
      climate_3 = stats::rnorm(n),
      climate_4 = stats::rnorm(n)
    )
  predictors <-
    list(
      human = c("human_1", "human_2", "human_3"),
      climate = c("climate_1", "climate_2", "climate_3", "climate_4")
    )

  result <-
    fit_spatial_hvarpart_group(
      data_group = data,
      response_vars = c("response_1", "response_2"),
      predictor_vars = predictors,
      permutations = 9L,
      alpha = 0.05,
      min_unique_locations = 10L,
      min_residual_df = 5L,
      distance_km = 500,
      seed = 343L
    )

  testthat::expect_true(
    result[["status"]] %in%
      c("no_spatial_terms_selected", "spatial_model_estimated")
  )
  testthat::expect_true(
    all(c("pure_human", "pure_climate", "pure_space") %in%
      result[["unique_adjusted_r2"]][["fraction"]])
  )
})
