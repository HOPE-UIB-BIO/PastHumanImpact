testthat::test_that(
  "diagnose_spatial_hvarpart_rankings() preserves no-signal rankings",
  {
    data_components <-
      tidyr::crossing(
        analysis = "temporal_spd",
        region = c("north", "south"),
        age = 2000,
        model_profile = "human_climate",
        predictor = c("human", "climate")
      ) |>
      dplyr::mutate(
        individual = dplyr::if_else(
          .data[["predictor"]] == "human",
          0.6,
          0.4
        )
      )
    data_status <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = c("north", "south"),
        age = 2000,
        status = c(
          "no_spatial_terms_selected",
          "spatial_not_estimable"
        ),
        n_selected = 0L
      )
    result <-
      diagnose_spatial_hvarpart_rankings(
        data_components = data_components,
        data_status = data_status
      )

    testthat::expect_equal(result$controlled_ranking[1], "human")
    testthat::expect_false(result$ranking_changed[1])
    testthat::expect_true(is.na(result$controlled_ranking[2]))
  }
)
