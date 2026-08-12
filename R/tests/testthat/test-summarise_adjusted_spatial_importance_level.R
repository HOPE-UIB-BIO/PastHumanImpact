testthat::test_that(
  "summarise_adjusted_spatial_importance_level() uses weights",
  {
    data_input <-
      tibble::tibble(
        region = c("a", "a"),
        adjusted_balance = c(-1, 1),
        weight = c(3, 1)
      )

    data_result <-
      summarise_adjusted_spatial_importance_level(
        data_adjusted = data_input,
        weight_col = "weight",
        group_vars = "region",
        level_name = "region",
        profile_name = "signed"
      )

    testthat::expect_equal(
      dplyr::pull(data_result, "adjusted_balance"),
      -0.5
    )
  }
)

testthat::test_that(
  "summarise_adjusted_spatial_importance_level() validates columns",
  {
    testthat::expect_error(
      summarise_adjusted_spatial_importance_level(
        data_adjusted = tibble::tibble(adjusted_balance = 0),
        weight_col = "missing",
        group_vars = character(),
        level_name = "overall",
        profile_name = "signed"
      ),
      "contract"
    )
  }
)
