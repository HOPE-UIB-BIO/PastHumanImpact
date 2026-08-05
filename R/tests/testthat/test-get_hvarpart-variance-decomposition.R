testthat::test_that("variance decomposition preserves signed accounting", {
  data_importance <-
    tibble::tibble(
      analysis = "spatial_spd",
      model_id = c("a", "a"),
      predictor = c("human", "climate"),
      individual = c(0.05, 0.15),
      varpart_unique = c(-0.02, 0.12),
      shared = c(0.1, 0.1),
      varpart_total = c(0.2, 0.2),
      total_adjusted_r_squared = c(0.2, 0.2),
      model_result_available = TRUE,
      varpart_available = TRUE,
      has_finite_varpart = TRUE
    )

  res_decomposition <-
    get_hvarpart_variance_decomposition(
      data_importance = data_importance,
      id_cols = c(
        "analysis",
        "model_id"
      )
    )

  testthat::expect_identical(nrow(res_decomposition), 1L)
  testthat::expect_equal(
    res_decomposition[["explained_component_sum"]],
    0.2
  )
  testthat::expect_equal(
    res_decomposition[["full_component_sum"]],
    1
  )
  testthat::expect_true(
    res_decomposition[["accounting_within_tolerance"]]
  )
  testthat::expect_true(
    res_decomposition[["has_negative_unique_human"]]
  )
  testthat::expect_equal(
    res_decomposition[["bounded_component_sum"]],
    1
  )
  testthat::expect_equal(
    res_decomposition[["bounded_unique_human"]],
    0
  )
})

testthat::test_that("variance decomposition flags unavailable Var.part", {
  data_importance <-
    tibble::tibble(
      model_id = c("a", "a"),
      predictor = c("human", "climate"),
      individual = c(0.05, 0.15),
      varpart_unique = NA_real_,
      shared = NA_real_,
      varpart_total = NA_real_,
      total_adjusted_r_squared = 0.2,
      model_result_available = TRUE,
      varpart_available = FALSE,
      has_finite_varpart = FALSE
    )

  res_decomposition <-
    get_hvarpart_variance_decomposition(
      data_importance = data_importance,
      id_cols = "model_id"
    )

  testthat::expect_false(res_decomposition[["has_finite_decomposition"]])
  testthat::expect_false(res_decomposition[["accounting_within_tolerance"]])
})
