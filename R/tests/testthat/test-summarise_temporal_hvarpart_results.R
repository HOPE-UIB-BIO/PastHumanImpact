testthat::test_that(
  "summarise_temporal_hvarpart_results() exports all result schemas",
  {
    data_dataset <-
      tibble::tibble(
        age = seq(500, 7000, 500),
        response_one = stats::rnorm(14),
        response_two = stats::rnorm(14),
        human = stats::rnorm(14),
        climate = stats::rnorm(14)
      )
    nested <-
      tibble::tibble(dataset_id = "a", data_merge = list(data_dataset))
    fitted <-
      fit_temporal_hvarpart_datasets(
        data_source = nested,
        response_vars = c("response_one", "response_two"),
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        permutations = 9L
      )
    result <-
      summarise_temporal_hvarpart_results(
        data_results = fitted,
        analysis = "spatial_spd"
      )

    testthat::expect_named(
      result,
      c("status", "components", "unique_adjusted_r2", "residual_moran")
    )
    testthat::expect_true(
      "human_climate_time" %in% result$components$model_profile
    )
  }
)
