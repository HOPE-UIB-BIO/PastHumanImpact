testthat::test_that(
  "prepare_spatial_hvarpart_composition() adds zero space for no signal",
  {
    components <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        model_profile = "human_climate",
        predictor = c("human", "climate"),
        individual = c(0.2, 0.8)
      )
    status <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        status = "no_spatial_terms_selected",
        selection_status = "no_spatial_signal"
      )
    result <- prepare_spatial_hvarpart_composition(components, status)

    testthat::expect_equal(sum(result$allocation), 1)
    testthat::expect_equal(
      result$allocation[result$predictor == "space"],
      0
    )
  }
)
