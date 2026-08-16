testthat::test_that(
  "H1 components follow the common long schema",
  {
    components <-
      tibble::tibble(
        dataset_id = c("a", "a", "a"),
        predictor = c("human", "climate", "time"),
        individual = c(0.2, 0.3, -0.1)
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

testthat::test_that(
  "H1 importance output satisfies the common result contract",
  {
    data_hierarchical <-
      data.frame(
        Unique = c(0.1, 0.2),
        Average.share = c(0.05, 0.05),
        Individual = c(0.15, 0.25),
        check.names = FALSE
      )

    data_hierarchical[["I.perc(%)"]] <-
      c(37.5, 62.5)

    rownames(data_hierarchical) <-
      c("human", "climate")

    data_varpart <-
      data.frame(
        Fractions = c(0.1, 0.2, 0.1, 0.4),
        check.names = FALSE
      )

    rownames(data_varpart) <-
      c(
        "Unique to human",
        "Unique to climate",
        "Common to human, and climate",
        "Total"
      )

    data_source <-
      tibble::tibble(
        dataset_id = "a",
        varhp = list(
          list(
            varhp_output = list(
              Hier.part = data_hierarchical,
              Var.part = data_varpart,
              Total_explained_variation = 0.4
            )
          )
        )
      )

    data_components <-
      compute_hvarpart_importance(
        data_source = data_source,
        id_cols = "dataset_id"
      )

    result <-
      prepare_h1_result_records(
        data_components = data_components,
        profile_id = "profile",
        model_specification = "human_climate_only",
        proxy = "spd",
        analytical_unit = "within_dataset",
        selected_control_dimensions = "none",
        input_hash = "input",
        profile_hash = "profile_hash",
        configuration_hash = "config"
      )

    testthat::expect_identical(
      result[["untruncated_value"]],
      data_components[["individual"]]
    )
    testthat::expect_equal(
      sum(result[["presentation_value"]]),
      1
    )
  }
)
