testthat::test_that("untruncated spatial summary retains human values", {
  data_importance <-
    tibble::tibble(
      analysis = rep("spatial_spd", 2L),
      model_id = rep("one", 2L),
      region = rep("Europe", 2L),
      predictor = c("human", "climate"),
      individual = c(0.4, 0.6),
      total_adjusted_r_squared = 1,
      has_negative_individual = FALSE,
      is_importance_eligible = TRUE
    )

  result <-
    summarise_untruncated_spatial_importance(
      data_importance = data_importance,
      group_vars = c("analysis", "region"),
      region_levels = "Europe"
    )

  testthat::expect_identical(result[["predictor"]], "human")

  testthat::expect_equal(result[["pooled_allocation"]], 0.4)
})
