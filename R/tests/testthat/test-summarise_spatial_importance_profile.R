testthat::test_that(
  "summarise_spatial_importance_profile() calculates weighted balance",
  {
    data_input <-
      tibble::tibble(
        region = c("a", "a"),
        climatezone = c("x", "y"),
        signed_balance = c(-1, 1),
        signed_weight = c(3, 1)
      )

    data_result <-
      summarise_spatial_importance_profile(
        data_subset = data_input,
        profile_name = "signed",
        balance_col = "signed_balance",
        weight_col = "signed_weight",
        group_vars = character(),
        level_name = "overall",
        sensitivity_type = "unthinned"
      )

    testthat::expect_equal(
      dplyr::pull(data_result, "importance_balance"),
      -0.5
    )
    testthat::expect_equal(dplyr::pull(data_result, "n_strata"), 2L)
  }
)

testthat::test_that(
  "summarise_spatial_importance_profile() validates columns",
  {
    testthat::expect_error(
      summarise_spatial_importance_profile(
        data_subset = tibble::tibble(region = "a"),
        profile_name = "signed",
        balance_col = "balance",
        weight_col = "weight",
        group_vars = character(),
        level_name = "overall",
        sensitivity_type = "unthinned"
      ),
      "contract"
    )
  }
)
