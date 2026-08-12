testthat::test_that(
  "summarise_spatial_importance_subset() returns all profiles and levels",
  {
    data_input <-
      tibble::tibble(
        region = c("a", "b"),
        climatezone = c("x", "y"),
        signed_balance = c(-0.5, 0.5),
        signed_weight = c(1, 1),
        zero_balance = c(-0.4, 0.4),
        zero_weight = c(1, 1)
      )

    data_result <-
      summarise_spatial_importance_subset(
        data_subset = data_input,
        sensitivity_type = "baseline"
      )

    testthat::expect_setequal(
      unique(dplyr::pull(data_result, "profile")),
      c("signed", "zero_truncated")
    )
    testthat::expect_setequal(
      unique(dplyr::pull(data_result, "aggregation_level")),
      c("overall", "region", "region_climatezone")
    )
  }
)

testthat::test_that(
  "summarise_spatial_importance_subset() validates input",
  {
    testthat::expect_error(
      summarise_spatial_importance_subset(
        data_subset = 1:3,
        sensitivity_type = "baseline"
      ),
      "contract"
    )
  }
)
