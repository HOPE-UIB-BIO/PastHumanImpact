testthat::test_that(
  "fit_temporal_hvarpart_dataset() fits time-control profiles",
  {
    set.seed(8)
    ages <- seq(500, 7000, 500)
    human <- stats::rnorm(14)
    climate <- stats::rnorm(14)
    data_input <-
      tibble::tibble(
        age = ages,
        response_one = human + ages / 7000 + stats::rnorm(14, sd = 0.1),
        response_two = climate + stats::rnorm(14, sd = 0.1),
        human = human,
        climate = climate
      )
    result <-
      fit_temporal_hvarpart_dataset(
        data_dataset = data_input,
        response_vars = c("response_one", "response_two"),
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        min_unique_ages = 10L,
        min_residual_df = 5L,
        permutations = 19L,
        seed = 8L
      )

    testthat::expect_true(
      result$status %in%
        c("estimated", "estimated_residual_temporal_dependence")
    )
    testthat::expect_false(is.null(result$temporal_hvarpart))
    testthat::expect_true("pure_time" %in% result$unique_adjusted_r2$fraction)
    testthat::expect_equal(
      sort(unique(result$residual_moran$stage)),
      c("human_climate_only", "time_controlled")
    )
  }
)

testthat::test_that(
  "fit_temporal_hvarpart_dataset() retains independent driver effects",
  {
    set.seed(900726)
    human <- stats::rnorm(14)
    climate <- stats::rnorm(14)
    data_input <-
      tibble::tibble(
        age = seq(500, 7000, 500),
        response_one = human,
        response_two = climate,
        human = human,
        climate = climate
      )
    result <-
      fit_temporal_hvarpart_dataset(
        data_dataset = data_input,
        response_vars = c("response_one", "response_two"),
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        permutations = 19L
      )
    pure_drivers <-
      result$unique_adjusted_r2 |>
      dplyr::filter(
        .data[["fraction"]] %in% c("pure_human", "pure_climate")
      ) |>
      dplyr::pull("adjusted_r_squared") |>
      sum()
    pure_time <-
      result$unique_adjusted_r2 |>
      dplyr::filter(.data[["fraction"]] == "pure_time") |>
      dplyr::pull("adjusted_r_squared")

    testthat::expect_gt(pure_drivers, 0.8)
    testthat::expect_lt(pure_time, 0.1)
  }
)

testthat::test_that(
  "fit_temporal_hvarpart_dataset() assigns a strong trend to time",
  {
    set.seed(900725)
    ages <- seq(500, 7000, 500)
    time_signal <- scale(ages)[, 1]
    data_input <-
      tibble::tibble(
        age = ages,
        response_one =
          time_signal + stats::rnorm(14, sd = 0.02),
        response_two =
          time_signal + stats::rnorm(14, sd = 0.02),
        human = stats::rnorm(14),
        climate = stats::rnorm(14)
      )
    result <-
      fit_temporal_hvarpart_dataset(
        data_dataset = data_input,
        response_vars = c("response_one", "response_two"),
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        permutations = 19L
      )
    time_contribution <-
      result$temporal_hvarpart$summary_table |>
      dplyr::filter(.data[["predictor"]] == "time") |>
      dplyr::pull("Individual")

    testthat::expect_gt(time_contribution, 0.5)
    testthat::expect_gt(
      result$unique_adjusted_r2 |>
        dplyr::filter(.data[["fraction"]] == "pure_time") |>
        dplyr::pull("adjusted_r_squared"),
      0.5
    )
  }
)

testthat::test_that(
  "fit_temporal_hvarpart_dataset() reports invalid age designs",
  {
    data_input <-
      tibble::tibble(
        age = seq(500, 7000, 500),
        response = stats::rnorm(14),
        human = stats::rnorm(14),
        climate = stats::rnorm(14)
      )
    data_missing <- data_input
    data_missing$age[1] <- NA_real_
    data_repeated <- data_input
    data_repeated$age[2] <- data_repeated$age[1]
    analyse_age_design <-
      purrr::partial(
        fit_temporal_hvarpart_dataset,
        response_vars = "response",
        predictor_vars = list(
          human = "human",
          climate = "climate"
        )
      )

    testthat::expect_equal(
      analyse_age_design(data_dataset = data_missing)$status,
      "incomplete_ages"
    )
    testthat::expect_equal(
      analyse_age_design(data_dataset = data_repeated)$status,
      "repeated_ages"
    )
  }
)

testthat::test_that(
  "fit_temporal_hvarpart_dataset() flags short series",
  {
    data_input <-
      tibble::tibble(
        age = seq(500, 4000, 500),
        response = seq_len(8),
        human = stats::rnorm(8),
        climate = stats::rnorm(8)
      )
    result <-
      fit_temporal_hvarpart_dataset(
        data_dataset = data_input,
        response_vars = "response",
        predictor_vars = list(
          human = "human",
          climate = "climate"
        )
      )

    testthat::expect_equal(result$status, "insufficient_unique_ages")
    testthat::expect_null(result$temporal_hvarpart)
  }
)
