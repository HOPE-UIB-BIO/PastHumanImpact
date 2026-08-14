testthat::test_that("definition hashes are deterministic and row-specific", {
  data_config <-
    tibble::tibble(
      model_id = c("model_a", "model_b"),
      formula_text = c("y ~ x", "y ~ s(x)"),
      input_data_hash = c("data_a", "data_b")
    )

  result <-
    compute_temporal_model_definition_hashes(
      data_config = data_config,
      definition_columns = c(
        "model_id",
        "formula_text",
        "input_data_hash"
      )
    )

  repeated <-
    compute_temporal_model_definition_hashes(
      data_config = data_config,
      definition_columns = c(
        "model_id",
        "formula_text",
        "input_data_hash"
      )
    )

  testthat::expect_identical(
    result[["definition_hash"]],
    repeated[["definition_hash"]]
  )
  testthat::expect_false(
    result[["definition_hash"]][1] == result[["definition_hash"]][2]
  )
})

testthat::test_that("definition hashing rejects missing columns", {
  testthat::expect_error(
    compute_temporal_model_definition_hashes(
      data_config = tibble::tibble(model_id = "model_a"),
      definition_columns = c("model_id", "formula_text")
    ),
    regexp = "missing"
  )
})
