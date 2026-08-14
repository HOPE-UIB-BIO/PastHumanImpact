testthat::test_that(
  "H1 components follow the common long schema",
  {
    components <-
      tibble::tibble(
        dataset_id = c("a", "a", "a"),
        predictor = c("human", "climate", "time"),
        Individual = c(0.2, 0.3, -0.1)
      )

    status <-
      tibble::tibble(
        dataset_id = "a",
        status = "estimated",
        design_rank = 3L,
        residual_df = 8L
      )

    result <-
      prepare_h1_result_records(
        data_components = components,
        data_status = status,
        profile_id = "profile",
        model_specification = "human_climate_time",
        proxy = "spd",
        analytical_unit = "within_dataset",
        selected_control_dimensions = "time",
        input_hash = "input",
        profile_hash = "profile_hash",
        configuration_hash = "config"
      )

    testthat::expect_equal(sum(result[["presentation_value"]]), 1)
    testthat::expect_equal(result[["presentation_value"]][3], 0)
    testthat::expect_true(all(result[["rank"]] == 3L))
    testthat::expect_true(
      all(
        c(
          "profile_id",
          "predictor_group",
          "untruncated_value",
          "presentation_value",
          "configuration_hash"
        ) %in% names(result)
      )
    )
  }
)
