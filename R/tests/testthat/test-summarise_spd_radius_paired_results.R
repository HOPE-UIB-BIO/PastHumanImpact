testthat::test_that("paired summaries use only matched continuous deltas", {
  data_source <-
    tibble::tibble(
      region = c("A", "A", "A"),
      profile_250_km_present = TRUE,
      profile_500_km_present = TRUE,
      estimable_250_km = c(TRUE, TRUE, FALSE),
      estimable_500_km = c(TRUE, TRUE, TRUE),
      matched_estimable = c(TRUE, TRUE, FALSE),
      status_changed = c(TRUE, FALSE, TRUE),
      ranking_reversal = c(TRUE, FALSE, FALSE),
      material_change = c(TRUE, FALSE, TRUE),
      robustness_classification = c(
        "ranking_reversal",
        "same_ranking",
        "gained_estimability"
      ),
      delta = c(2, 4, 100)
    )

  result <-
    summarise_spd_radius_paired_results(
      data_comparison = data_source,
      delta_col = "delta",
      group_cols = "region",
      summary_level = "region"
    )

  testthat::expect_identical(result[["n_matched_estimable"]], 2L)
  testthat::expect_equal(result[["median_delta"]], 3)
  testthat::expect_equal(result[["proportion_ranking_reversals"]], 0.5)
  testthat::expect_identical(result[["n_gained_estimability"]], 1L)
  testthat::expect_equal(result[["proportion_material_changes"]], 2 / 3)
})
