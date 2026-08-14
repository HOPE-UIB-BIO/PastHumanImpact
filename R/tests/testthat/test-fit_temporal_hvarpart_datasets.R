testthat::test_that(
  "fit_temporal_hvarpart_datasets() retains one result per dataset",
  {
    data_dataset <-
      tibble::tibble(
        age = seq(500, 7000, 500),
        response = stats::rnorm(14),
        human = stats::rnorm(14),
        climate = stats::rnorm(14)
      )
    data_input <-
      tibble::tibble(
        dataset_id = c("a", "b"),
        data_merge = list(data_dataset, data_dataset)
      )
    result <-
      fit_temporal_hvarpart_datasets(
        data_source = data_input,
        response_vars = "response",
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        permutations = 9L
      )

    testthat::expect_equal(nrow(result), 2L)
    testthat::expect_true(is.list(result$result))
  }
)
