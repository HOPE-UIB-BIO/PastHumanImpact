testthat::test_that(
  "resolve_hvarpart_predictor_vars() supports regional resolver functions",
  {
    resolver <- function(region, available_columns) {
      resolve_region_event_predictor_specification(
        region = region,
        proxy_variant = "events",
        available_columns = available_columns
      )
    }
    result <- resolve_hvarpart_predictor_vars(
      predictor_vars = resolver,
      region = "North America",
      available_columns = c(
        "fc",
        "es",
        "temp_annual",
        "temp_cold",
        "prec_summer",
        "prec_win"
      )
    )

    testthat::expect_identical(result[["human"]], c("fc", "es"))
    testthat::expect_identical(
      result[["climate"]],
      c("temp_annual", "temp_cold", "prec_summer", "prec_win")
    )
  }
)

testthat::test_that(
  "resolve_hvarpart_predictor_vars() rejects duplicated mappings",
  {
    testthat::expect_error(
      resolve_hvarpart_predictor_vars(
        predictor_vars = list(
          human = c("spd", "spd"),
          climate = "temp_annual"
        ),
        region = "Europe",
        available_columns = c("spd", "temp_annual")
      ),
      regexp = "unique and non-overlapping"
    )
  }
)
