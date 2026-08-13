testthat::test_that("correlations require sample size and variation", {
  data_values <-
    tibble::tibble(
      group = c(rep("valid", 3), rep("constant", 3)),
      adjusted_r_squared = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
      human_importance_bounded = c(0.2, 0.4, 0.8, 0.5, 0.5, 0.5)
    )

  res_stats <-
    summarise_hvarpart_correlations(
      data_values = data_values,
      group_vars = "group",
      importance_column = "human_importance_bounded"
    )

  testthat::expect_true(
    res_stats[["correlation_available"]][
      res_stats[["group"]] == "valid"
    ]
  )

  testthat::expect_false(
    res_stats[["correlation_available"]][
      res_stats[["group"]] == "constant"
    ]
  )

  testthat::expect_true(
    is.na(
      res_stats[["spearman_rho"]][
        res_stats[["group"]] == "constant"
      ]
    )
  )
})
