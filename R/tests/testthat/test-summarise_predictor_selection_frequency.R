testthat::test_that("summarise_predictor_selection_frequency() computes rates", {
  data_selection <- tibble::tibble(
    dataset_id = c("a", "b", "a", "b"),
    region = "Europe",
    group = c("human", "human", "climate", "climate"),
    predictor = c("spd_sqrt", "spd_sqrt", "temp_annual", "temp_annual"),
    selected = c(TRUE, FALSE, TRUE, TRUE)
  )
  result <- summarise_predictor_selection_frequency(
    selection_records = data_selection,
    grouping_variable = "region"
  )
  human <- result |>
    dplyr::filter(.data[["group"]] == "human")
  testthat::expect_equal(dplyr::pull(human, retention_rate), 0.5)
  testthat::expect_equal(dplyr::pull(human, n_datasets), 2L)
})

testthat::test_that("summarise_predictor_selection_frequency() validates inputs", {
  testthat::expect_error(
    summarise_predictor_selection_frequency(
      selection_records = data.frame(dataset_id = "a"),
      grouping_variable = "region"
    ),
    "contract"
  )
})
