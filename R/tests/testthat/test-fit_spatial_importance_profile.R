testthat::test_that(
  "fit_spatial_importance_profile() fits a weighted model",
  {
    data_input <-
      tibble::tibble(
        spatial_stratum = factor(rep(c("a", "b"), each = 4)),
        signed_balance = seq(-0.8, 0.6, length.out = 8),
        signed_weight = rep(1, 8),
        dbmem_001 = rep(c(-1, 1), 4)
      )

    model_result <-
      fit_spatial_importance_profile(
        data_model = data_input,
        response_col = "signed_balance",
        weight_col = "signed_weight",
        selected_mem_names = "dbmem_001"
      )

    testthat::expect_s3_class(model_result, "lm")
    testthat::expect_equal(length(stats::fitted(model_result)), 8L)
  }
)

testthat::test_that(
  "fit_spatial_importance_profile() validates selected terms",
  {
    testthat::expect_error(
      fit_spatial_importance_profile(
        data_model = tibble::tibble(
          spatial_stratum = factor(c("a", "b")),
          balance = c(0, 1),
          weight = c(1, 1)
        ),
        response_col = "balance",
        weight_col = "weight",
        selected_mem_names = "missing"
      ),
      "contract"
    )
  }
)
