testthat::test_that(
  "temporal config columns follow candidate types",
  {
    testthat::expect_identical(
      cast_temporal_config_column(
        current_column = as.Date("2026-01-01"),
        candidate_column = NA_character_
      ),
      "2026-01-01"
    )

    testthat::expect_identical(
      cast_temporal_config_column(
        current_column = 2,
        candidate_column = NA_integer_
      ),
      2L
    )
  }
)
