testthat::test_that(
  "summarise_spatial_hvarpart_balance() pools predictor allocations",
  {
    data_input <-
      tibble::tibble(
        analysis = "spatial_spd",
        model_id = rep(c("one", "two"), each = 2),
        region = "Europe",
        predictor = rep(c("human", "climate"), 2),
        individual = c(0.7, 0.3, 0.5, 0.5),
        total_adjusted_r_squared = 1,
        has_negative_individual = FALSE,
        is_importance_eligible = TRUE
      )
    result <-
      summarise_spatial_hvarpart_balance(
        data_importance = data_input,
        group_vars = c("analysis", "region"),
        region_levels = "Europe"
      )

    testthat::expect_s3_class(result, "tbl_df")
    testthat::expect_equal(result[["importance_balance"]], 0.2)
  }
)
