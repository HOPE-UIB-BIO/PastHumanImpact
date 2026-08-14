testthat::test_that(
  "temporal configuration casting follows candidate schema",
  {
    current <-
      tibble::tibble(
        model_id = "model_a",
        attempt = 1,
        run_date = as.Date("2026-01-01")
      )

    candidate <-
      tibble::tibble(
        model_id = "model_a",
        attempt = 1L,
        run_date = NA_character_
      )

    result <-
      cast_temporal_model_configuration(
        data_current = current,
        data_candidate = candidate
      )

    testthat::expect_identical(result[["attempt"]], 1L)
    testthat::expect_identical(result[["run_date"]], "2026-01-01")
  }
)
