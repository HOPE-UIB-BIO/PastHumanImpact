testthat::test_that(
  "analyse_spatial_hvarpart_group() returns all spatial profiles",
  {
    set.seed(900723)
    n_rows <- 36L
    spatial_gradient <- scale(seq_len(n_rows))[, 1]
    human <- stats::rnorm(n_rows)
    climate <- stats::rnorm(n_rows)
    data_input <-
      tibble::tibble(
        dataset_id = seq_len(n_rows),
        long = seq(0, 3.5, by = 0.1),
        lat = 45,
        response_one = spatial_gradient + 0.3 * human,
        response_two = spatial_gradient + 0.3 * climate,
        response_three = spatial_gradient + stats::rnorm(n_rows, sd = 0.1),
        human = human,
        climate = climate
      )

    result <-
      analyse_spatial_hvarpart_group(
        data_group = data_input,
        response_vars = c(
          "response_one",
          "response_two",
          "response_three"
        ),
        predictor_vars = list(
          human = "human",
          climate = "climate"
        ),
        permutations = 19L,
        min_unique_locations = 10L,
        min_residual_df = 5L,
        distance_km = 50,
        seed = 900723L
      )

    testthat::expect_type(result, "list")
    testthat::expect_true(
      result[["status"]] %in% c(
        "spatial_model_estimated",
        "no_spatial_terms_selected"
      )
    )
    testthat::expect_equal(
      dplyr::pull(result[["partial_fractions"]], "fraction"),
      c(
        "pure_human",
        "pure_climate",
        "pure_space",
        "shared",
        "total_explained",
        "unexplained"
      )
    )
    testthat::expect_true(
      all(
        unique(dplyr::pull(result[["residual_moran"]], "value")) %in%
          c("residual_axis_1", "residual_axis_2", "residual_axis_3")
      )
    )
  }
)

testthat::test_that(
  "analyse_spatial_hvarpart_group() flags insufficient spatial locations",
  {
    set.seed(900723)
    data_input <-
      tibble::tibble(
        dataset_id = rep(1:6, each = 2),
        long = rep(seq(0, 0.5, by = 0.1), each = 2),
        lat = 45,
        response_one = stats::rnorm(12),
        response_two = stats::rnorm(12),
        human = stats::rnorm(12),
        climate = stats::rnorm(12)
      )

    result <-
      analyse_spatial_hvarpart_group(
        data_group = data_input,
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

    testthat::expect_equal(result[["status"]], "spatial_not_estimable")
    testthat::expect_null(result[["spatial_hvarpart"]])
    testthat::expect_equal(
      dplyr::pull(result[["partial_fractions"]], "adjusted_r_squared")[3],
      0
    )
  }
)
