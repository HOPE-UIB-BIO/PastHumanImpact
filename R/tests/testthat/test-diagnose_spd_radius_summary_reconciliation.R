testthat::test_that("summary reconciliation verifies grouped source values", {
  paired <-
    tibble::tibble(
      region = c("A", "A", "B"),
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
  summary <-
    dplyr::bind_rows(
      summarise_spd_radius_paired_results(
        paired,
        delta_col = "delta",
        summary_level = "overall"
      ),
      summarise_spd_radius_paired_results(
        paired,
        delta_col = "delta",
        group_cols = "region",
        summary_level = "region"
      )
    )

  result <-
    diagnose_spd_radius_summary_reconciliation(paired, summary, "delta")

  testthat::expect_true(all(result[["all_values_reconciled"]]))
})

testthat::test_that("summary reconciliation rejects altered counts", {
  paired <-
    tibble::tibble(
      profile_250_km_present = TRUE,
      profile_500_km_present = TRUE,
      estimable_250_km = TRUE,
      estimable_500_km = TRUE,
      matched_estimable = TRUE,
      status_changed = FALSE,
      ranking_reversal = FALSE,
      material_change = FALSE,
      robustness_classification = "same_ranking",
      delta = 1
    )
  summary <-
    summarise_spd_radius_paired_results(paired, "delta") |>
    dplyr::mutate(n_units = 2L)

  testthat::expect_error(
    diagnose_spd_radius_summary_reconciliation(paired, summary, "delta"),
    regexp = "disagrees"
  )
})

testthat::test_that("summary reconciliation rejects altered proportions", {
  paired <-
    tibble::tibble(
      profile_250_km_present = TRUE,
      profile_500_km_present = TRUE,
      estimable_250_km = TRUE,
      estimable_500_km = TRUE,
      matched_estimable = TRUE,
      status_changed = TRUE,
      ranking_reversal = TRUE,
      material_change = TRUE,
      robustness_classification = "ranking_reversal",
      delta = 1
    )
  summary <-
    summarise_spd_radius_paired_results(paired, "delta") |>
    dplyr::mutate(proportion_ranking_reversals = 0)

  testthat::expect_error(
    diagnose_spd_radius_summary_reconciliation(paired, summary, "delta"),
    regexp = "disagrees"
  )
})
