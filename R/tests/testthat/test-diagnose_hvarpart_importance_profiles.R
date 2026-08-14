testthat::test_that("profile comparison reports signed differences", {
  data_importance <-
    tibble::tibble(
      region = rep("Europe", 4),
      model_id = rep(c("a", "b"), each = 2),
      predictor = rep(c("human", "climate"), 2),
      individual = c(-0.02, 0.12, 0.2, 0.3),
      total_adjusted_r_squared = c(0.1, 0.1, 0.5, 0.5),
      has_negative_individual = c(TRUE, TRUE, FALSE, FALSE),
      is_importance_eligible = TRUE
    )

  data_comparison <-
    diagnose_hvarpart_importance_profiles(
      data_importance = data_importance,
      group_vars = "region"
    )

  testthat::expect_setequal(
    unique(data_comparison[["profile"]]),
    c("signed", "zero_truncated", "exclude_negative")
  )
  testthat::expect_equal(
    data_comparison[["delta_from_signed"]][
      data_comparison[["profile"]] == "signed"
    ],
    c(0, 0)
  )
})
