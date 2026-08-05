testthat::test_that("correlation values retain bounded and signed profiles", {
  data_importance <-
    tidyr::crossing(
      model_id = c("a", "b"),
      predictor = c("human", "climate")
    ) |>
    dplyr::arrange(.data[["model_id"]], .data[["predictor"]]) |>
    dplyr::mutate(
      individual = dplyr::case_when(
        .data[["model_id"]] == "a" & .data[["predictor"]] == "human" ~
          0.15,
        .data[["model_id"]] == "a" ~ -0.05,
        .data[["predictor"]] == "human" ~ 0.05,
        .default = 0.15
      ),
      total_adjusted_r_squared = c(0.1, 0.1, 0.2, 0.2),
      is_importance_eligible = TRUE
    )

  res_values <-
    get_hvarpart_correlation_values(
      data_importance = data_importance,
      id_cols = "model_id"
    )

  testthat::expect_identical(nrow(res_values), 2L)
  testthat::expect_equal(
    res_values[["human_importance_bounded"]],
    c(1, 0.25)
  )
  testthat::expect_equal(
    res_values[["human_importance_signed"]],
    c(1.5, 0.25)
  )
})

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
