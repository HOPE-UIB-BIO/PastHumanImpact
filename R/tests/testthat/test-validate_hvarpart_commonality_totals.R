testthat::test_that("validate_hvarpart_commonality_totals() passes totals", {
  data_commonality <-
    tibble::tibble(
      model_id = "m",
      dataset_id = c("a", "a", "b", "b"),
      fraction = c(0.2, 0.1, 0.05, 0.05),
      total_fraction = c(0.3, 0.3, 0.1, 0.1)
    )

  data_components <-
    tibble::tibble(
      model_id = "m",
      dataset_id = c("a", "a", "b", "b"),
      model_profile = c(
        "human_climate_time", "human_climate",
        "human_climate_time", "human_climate"
      ),
      total_adjusted_r_squared = c(0.3004, 0.25, 0.1, 0.08)
    )

  result <-
    validate_hvarpart_commonality_totals(
      data_commonality = data_commonality,
      data_components = data_components
    )

  testthat::expect_named(
    result,
    c(
      "model_id", "dataset_id", "sum_fractions", "total_fraction",
      "total_adjusted_r_squared", "difference_total",
      "difference_component", "passed"
    )
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(dplyr::pull(result, "passed")))
})

testthat::test_that("validate_hvarpart_commonality_totals() aborts", {
  data_commonality <-
    tibble::tibble(
      model_id = "m",
      dataset_id = "a",
      fraction = c(0.2, 0.1),
      total_fraction = 0.3
    )

  data_components_off <-
    tibble::tibble(
      model_id = "m",
      dataset_id = "a",
      model_profile = "human_climate_time",
      total_adjusted_r_squared = 0.35
    )

  testthat::expect_error(
    validate_hvarpart_commonality_totals(
      data_commonality = data_commonality,
      data_components = data_components_off
    ),
    "commonality"
  )

  data_components_missing <-
    dplyr::mutate(data_components_off, dataset_id = "z")

  testthat::expect_error(
    validate_hvarpart_commonality_totals(
      data_commonality = data_commonality,
      data_components = data_components_missing
    ),
    "commonality"
  )
  testthat::expect_error(
    validate_hvarpart_commonality_totals(
      data_commonality = data_commonality,
      data_components = tibble::tibble(model_id = "m")
    )
  )
})
