testthat::test_that("summarise_predictor_selection_sets() counts local sets", {
  data_selection <- tibble::tibble(
    dataset_id = c("a", "b", "c"),
    region = c("Europe", "Europe", "Europe"),
    selected_predictors_human = c(
      "spd_sqrt", "spd_sqrt", "spd_sqrt + hyde_sqrt"
    ),
    selected_predictors_climate = c("temp_annual", "temp_annual", "prec_win")
  )
  result <- summarise_predictor_selection_sets(
    selection_by_dataset = data_selection,
    grouping_variable = "region"
  )
  human_spd <- result |>
    dplyr::filter(
      .data[["group"]] == "human",
      .data[["selected_predictors"]] == "spd_sqrt"
    )
  testthat::expect_equal(dplyr::pull(human_spd, n_datasets), 2L)
  testthat::expect_equal(dplyr::pull(human_spd, proportion), 2 / 3)
})

testthat::test_that("summarise_predictor_selection_sets() validates inputs", {
  testthat::expect_error(
    summarise_predictor_selection_sets(
      selection_by_dataset = data.frame(region = "Europe"),
      grouping_variable = "region"
    ),
    "contract"
  )
})
