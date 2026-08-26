testthat::test_that(
  "prepare_hvarpart_control_components() normalises component names",
  {
    data_result <-
      list(
        human_climate_only_hvarpart = list(
          summary_table = tibble::tibble(
            predictor = "human",
            Individual = 0.2
          ),
          varhp_output = list(
            Total_explained_variation = 0.3
          )
        ),
        temporal_hvarpart = list(
          summary_table = tibble::tibble(
            predictor = "human",
            Individual = 0.4
          ),
          varhp_output = list(
            Total_explained_variation = 0.5
          )
        )
      )

    result <-
      prepare_hvarpart_control_components(
        data_result = data_result,
        controlled_result_name = "temporal_hvarpart",
        controlled_profile = "human_climate_time",
        include_total = TRUE
      )

    testthat::expect_true("individual" %in% names(result))
    testthat::expect_false("Individual" %in% names(result))
    testthat::expect_identical(
      result[["model_profile"]],
      c("human_climate", "human_climate_time")
    )
    testthat::expect_equal(
      result[["total_adjusted_r_squared"]],
      c(0.3, 0.5)
    )
  }
)

testthat::test_that(
  "prepare_hvarpart_control_components() retains missing profiles",
  {
    result <-
      prepare_hvarpart_control_components(
        data_result = list(
          human_climate_only_hvarpart = NULL,
          spatial_hvarpart = NULL
        ),
        controlled_result_name = "spatial_hvarpart",
        controlled_profile = "human_climate_space"
      )

    testthat::expect_s3_class(result, "tbl_df")
    testthat::expect_equal(nrow(result), 0L)
  }
)
