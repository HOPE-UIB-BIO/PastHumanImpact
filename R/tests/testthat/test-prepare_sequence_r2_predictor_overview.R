testthat::test_that("prepare_sequence_r2_predictor_overview() pairs datasets", {
  canonical_balance <- tibble::tibble(
    dataset_id = c("a", "b"), total_adjusted_r_squared = c(0.2, 0.4),
    signed_balance = c(-0.2, 0.1), region = c("Europe", "Asia"),
    climatezone = c("Polar", "Arid"), long = c(1, 2), lat = c(3, 4)
  )
  filtered_balance <- canonical_balance |>
    dplyr::mutate(
      total_adjusted_r_squared = c(0.3, 0.5),
      signed_balance = c(0.1, 0.2)
    )
  canonical_components <- tidyr::expand_grid(
    dataset_id = c("a", "b"),
    component = c("human", "climate", "time")
  ) |>
    dplyr::mutate(measure = "unique_adjusted_r2", value = 0.1)
  filtered_unique <- tidyr::expand_grid(
    dataset_id = c("a", "b"),
    fraction = c("pure_human", "pure_climate", "pure_time")
  ) |>
    dplyr::mutate(adjusted_r_squared = 0.15)
  predictor_selection <- tidyr::expand_grid(
    dataset_id = c("a", "b"),
    predictor = c("spd_sqrt", "temp_annual")
  ) |>
    dplyr::mutate(
      group = ifelse(.data[["predictor"]] == "spd_sqrt", "human", "climate"),
      preference_rank = 1L, selected = TRUE, reason = "selected"
    )
  result <- prepare_sequence_r2_predictor_overview(
    canonical_balance = canonical_balance,
    canonical_components = canonical_components,
    filtered_balance = filtered_balance,
    filtered_unique_r2 = filtered_unique,
    predictor_selection = predictor_selection
  )
  testthat::expect_named(result, c(
    "sequence_values", "comparison_long", "r2_summary",
    "selection_by_dataset", "selection_frequency_by_continental_region",
    "selection_frequency_by_region",
    "selection_sets_by_continental_region", "selection_sets_by_region"
  ))
  testthat::expect_equal(nrow(result[["sequence_values"]]), 2L)
  testthat::expect_equal(
    dplyr::pull(result[["sequence_values"]], delta_total),
    c(0.1, 0.1)
  )
  testthat::expect_equal(nrow(result[["comparison_long"]]), 8L)
})

testthat::test_that("prepare_sequence_r2_predictor_overview() rejects duplicates", {
  balance <- tibble::tibble(
    dataset_id = c("a", "a"), total_adjusted_r_squared = 0.2,
    signed_balance = 0, region = "Europe", climatezone = "Polar",
    long = 1, lat = 2
  )
  components <- tibble::tibble(
    dataset_id = "a", measure = "unique_adjusted_r2",
    component = "human", value = 0.1
  )
  unique_r2 <- tibble::tibble(
    dataset_id = "a", fraction = "pure_human", adjusted_r_squared = 0.1
  )
  selection <- tibble::tibble(
    dataset_id = "a", group = "human", predictor = "spd_sqrt",
    preference_rank = 1L, selected = TRUE, reason = "selected"
  )
  testthat::expect_error(
    prepare_sequence_r2_predictor_overview(
      balance, components, balance, unique_r2, selection
    ),
    "one row per dataset_id"
  )
})
