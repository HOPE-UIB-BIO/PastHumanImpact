testthat::test_that("shared climate selections are audited and enforced", {
  designs <- tibble::tibble(
    dataset_id = rep(c("a", "b"), each = 2L),
    model_id = rep(c("joint_filtered", "spd_matched_bridge"), 2L),
    predictor_vars = list(
      list(human = "spd_sqrt", climate = c("temp_cold", "prec_win")),
      list(human = "spd_sqrt", climate = c("temp_cold", "prec_win")),
      list(human = "kk10_fraction", climate = "temp_annual"),
      list(human = "spd_sqrt", climate = "temp_annual")
    )
  )
  audit <- validate_shared_local_climate_selections(designs, "dataset_id")
  testthat::expect_equal(nrow(audit), 2L)
  testthat::expect_true(all(audit$identical_climate_set))

  designs$predictor_vars[[4]]$climate <- "prec_summer"
  testthat::expect_error(
    validate_shared_local_climate_selections(designs, "dataset_id"),
    "Climate predictors must be identical"
  )
})
