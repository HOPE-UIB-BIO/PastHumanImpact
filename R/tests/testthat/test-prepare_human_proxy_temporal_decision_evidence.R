testthat::test_that("temporal decision evidence uses common region-age units", {
  canonical <- tidyr::crossing(
    analysis = "temporal_spd", region = "Europe", age = c(1500, 2000),
    predictor = c("human", "climate", "space")
  ) |>
    dplyr::mutate(
      allocation = c(0.2, 0.6, 0.2, 0.2, 0.6, 0.2),
      Unique = 0.1,
      spatial_adjusted_r_squared = 0.4
    )
  matched <- canonical |>
    dplyr::filter(.data[["age"]] == 2000) |>
    dplyr::select(-dplyr::all_of("analysis"))
  result <- prepare_human_proxy_temporal_decision_evidence(
    canonical, matched, matched
  )
  testthat::expect_equal(nrow(result[["temporal_common_cohort"]]), 9L)
  testthat::expect_equal(
    dplyr::pull(result[["temporal_summary"]], n_units), rep(1L, 3L)
  )
  testthat::expect_error(
    prepare_human_proxy_temporal_decision_evidence(
      canonical, matched, matched, age_min = 8000, age_max = 2000
    ),
    "contract"
  )
})
