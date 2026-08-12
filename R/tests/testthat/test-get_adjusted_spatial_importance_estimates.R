testthat::test_that(
  "get_adjusted_spatial_importance_estimates() sets dbMEMs to zero",
  {
    data_input <-
      tibble::tibble(
        spatial_stratum = factor(rep(c("a", "b"), each = 4)),
        region = rep(c("a", "b"), each = 4),
        climatezone = rep(c("x", "y"), each = 4),
        signed_balance = seq(-0.8, 0.6, length.out = 8),
        signed_weight = rep(1, 8),
        dbmem_001 = rep(c(-1, 1), 4)
      )
    model_input <-
      fit_spatial_importance_profile(
        data_model = data_input,
        response_col = "signed_balance",
        weight_col = "signed_weight",
        selected_mem_names = "dbmem_001"
      )

    data_result <-
      get_adjusted_spatial_importance_estimates(
        model_object = model_input,
        data_model = data_input,
        profile_name = "signed",
        weight_col = "signed_weight",
        mem_names = "dbmem_001"
      )

    testthat::expect_s3_class(data_result, "tbl_df")
    testthat::expect_setequal(
      unique(dplyr::pull(data_result, "aggregation_level")),
      c("overall", "region", "region_climatezone")
    )
  }
)

testthat::test_that(
  "get_adjusted_spatial_importance_estimates() validates models",
  {
    testthat::expect_error(
      get_adjusted_spatial_importance_estimates(
        model_object = list(),
        data_model = tibble::tibble(),
        profile_name = "signed",
        weight_col = "weight"
      ),
      "contract"
    )
  }
)
