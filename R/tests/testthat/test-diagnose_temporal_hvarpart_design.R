testthat::test_that(
  "diagnose_temporal_hvarpart_design() enforces residual degrees of freedom",
  {
    data_input <-
      tibble::tibble(
        age = seq(500, 6000, 500),
        response = stats::rnorm(12),
        human = stats::rnorm(12),
        climate = stats::rnorm(12),
        time = scale(seq_len(12))[, 1]
      )
    result <-
      diagnose_temporal_hvarpart_design(
        data_source = data_input,
        response_vars = "response",
        predictor_vars = list(
          human = "human",
          climate = "climate",
          time = "time"
        ),
        min_unique_ages = 10L,
        min_residual_df = 5L
      )

    testthat::expect_equal(result[["status"]], "estimable")
    testthat::expect_gte(result[["residual_df"]], 5L)
  }
)

testthat::test_that(
  "diagnose_temporal_hvarpart_design() identifies rank deficiency",
  {
    data_input <-
      tibble::tibble(
        age = seq(500, 6000, 500),
        response = stats::rnorm(12),
        human = seq_len(12),
        climate = seq_len(12),
        time = scale(seq_len(12))[, 1]
      )
    result <-
      diagnose_temporal_hvarpart_design(
        data_source = data_input,
        response_vars = "response",
        predictor_vars = list(
          human = "human",
          climate = "climate",
          time = "time"
        ),
        min_unique_ages = 10L,
        min_residual_df = 5L
      )

    testthat::expect_equal(result[["status"]], "rank_deficient")
    testthat::expect_false(result[["design_full_rank"]])
  }
)
