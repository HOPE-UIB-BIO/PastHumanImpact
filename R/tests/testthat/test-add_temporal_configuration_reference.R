testthat::test_that(
  "temporal policy references preserve persisted values",
  {
    config <-
      tibble::tibble(
        model_id = c("a", "b"),
        configuration_reference_hash = c("old", NA_character_)
      )

    result <-
      add_temporal_configuration_reference(
        data_config = config,
        reference_hash = "new"
      )

    testthat::expect_identical(
      result[["configuration_reference_hash"]],
      c("old", "new")
    )
  }
)
