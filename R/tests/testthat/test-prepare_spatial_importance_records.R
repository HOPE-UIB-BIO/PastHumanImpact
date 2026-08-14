testthat::test_that(
  "prepare_spatial_importance_records() preserves canonical weighting",
  {
    data_importance <-
      tibble::tibble(
        dataset_id = rep(1:2, each = 2),
        model_id = rep(c("m1", "m2"), each = 2),
        predictor = rep(c("human", "climate"), 2),
        individual = c(0.2, 0.3, -0.1, 0.5),
        total_adjusted_r_squared = rep(c(0.5, 0.4), each = 2),
        is_importance_eligible = TRUE
      )
    data_meta <-
      tibble::tibble(
        dataset_id = 1:2,
        long = c(0, 1),
        lat = c(50, 51),
        region = "Europe",
        climatezone = c("cold", "warm")
      )

    result <-
      prepare_spatial_importance_records(
        data_importance = data_importance,
        data_meta = data_meta
      )

    testthat::expect_equal(nrow(result), 2L)
    testthat::expect_equal(
      dplyr::pull(result, "signed_balance"),
      c(-0.2, -1.5)
    )
    testthat::expect_equal(
      dplyr::pull(result, "zero_balance"),
      c(-0.2, -1)
    )
    testthat::expect_equal(
      dplyr::pull(result, "signed_weight"),
      c(0.5, 0.4)
    )
  }
)

testthat::test_that(
  "prepare_spatial_importance_records() rejects incomplete predictor pairs",
  {
    data_importance <-
      tibble::tibble(
        dataset_id = 1,
        model_id = "m1",
        predictor = "human",
        individual = 0.2,
        total_adjusted_r_squared = 0.5,
        is_importance_eligible = TRUE
      )
    data_meta <-
      tibble::tibble(
        dataset_id = 1,
        long = 0,
        lat = 50,
        region = "Europe",
        climatezone = "cold"
      )

    testthat::expect_error(
      prepare_spatial_importance_records(
        data_importance = data_importance,
        data_meta = data_meta
      ),
      "finite human and climate"
    )
  }
)
