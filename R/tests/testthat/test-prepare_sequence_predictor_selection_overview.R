testthat::test_that("prepare_sequence_predictor_selection_overview() summarises", {
  balance <- tibble::tibble(
    dataset_id = "a", region = "Europe", climatezone = "Polar",
    long = 1, lat = 2
  )
  selection <- tibble::tibble(
    dataset_id = "a", group = c("human", "climate"),
    predictor = c("spd_sqrt", "temp_annual"),
    preference_rank = 1L, selected = TRUE, reason = "retained"
  )
  result <- prepare_sequence_predictor_selection_overview(balance, selection)
  testthat::expect_named(result, c(
    "selection_by_dataset", "selection_frequency_by_continental_region",
    "selection_frequency_by_region", "selection_sets_by_continental_region",
    "selection_sets_by_region"
  ))
  testthat::expect_equal(nrow(result[["selection_by_dataset"]]), 1L)
  testthat::expect_error(
    prepare_sequence_predictor_selection_overview(
      balance, dplyr::bind_rows(selection, selection)
    ),
    "one row per dataset"
  )
})
