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
    compute_hvarpart_correlation_values(
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
