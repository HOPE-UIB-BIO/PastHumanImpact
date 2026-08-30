testthat::test_that(
  "prepare_spatial_hvarpart_contributions() retains no-spatial-term slices",
  {
    components <-
      tibble::tibble(
        analysis = "temporal_events",
        region = "Oceania",
        age = 4500,
        model_profile = "human_climate",
        predictor = c("human", "climate"),
        individual = c(0.35, 0.65)
      )
    status <-
      tibble::tibble(
        analysis = "temporal_events",
        region = "Oceania",
        age = 4500,
        status = "no_spatial_terms_selected"
      )

    result <-
      prepare_spatial_hvarpart_contributions(components, status)

    testthat::expect_equal(nrow(result), 3L)
    testthat::expect_setequal(
      result[["predictor"]],
      c("human", "climate", "space")
    )
    testthat::expect_equal(
      result[["individual"]][result[["predictor"]] == "space"],
      0
    )
    testthat::expect_true(
      all(result[["model_profile"]] == "human_climate_space")
    )
  }
)
