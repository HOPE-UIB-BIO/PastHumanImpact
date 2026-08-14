testthat::test_that(
  "prepare_human_climate_only_records() restores the common schema",
  {
    data_input <-
      tibble::tibble(
        model_id = c("a", "b"),
        region = "Europe",
        climatezone = "Temperate",
        human_climate_only_signed_balance = c(0.4, Inf),
        human_climate_only_signed_weight = c(0.5, 0),
        human_climate_only_zero_balance = c(0.4, NA_real_),
        human_climate_only_zero_weight = c(1, 0)
      )
    result <- prepare_human_climate_only_records(data_input)

    testthat::expect_equal(result$model_id, "a")
    testthat::expect_equal(result$signed_balance, 0.4)
    testthat::expect_equal(result$zero_balance, 0.4)
  }
)
