testthat::test_that(
  "fit_spatial_hvarpart_dataset() retains region-age identifiers",
  {
    data_group <-
      tibble::tibble(
        dataset_id = seq_len(12),
        long = seq(0, 1.1, by = 0.1),
        lat = 45,
        response_one = stats::rnorm(12),
        response_two = stats::rnorm(12),
        human = stats::rnorm(12),
        climate = stats::rnorm(12)
      )
    data_input <-
      tibble::tibble(
        region = "Europe",
        age = 2000,
        data_merge = list(data_group)
      )
    result <-
      fit_spatial_hvarpart_dataset(
        data_source = data_input,
        analysis = "temporal_spd",
        response_vars = c("response_one", "response_two"),
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        permutations = 9L,
        min_unique_locations = 20L,
        min_residual_df = 3L,
        distance_km = 50
      )

    testthat::expect_equal(result$analysis, "temporal_spd")
    testthat::expect_equal(result$region, "Europe")
    testthat::expect_true(is.list(result$result))
  }
)
