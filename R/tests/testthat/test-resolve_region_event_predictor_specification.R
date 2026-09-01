testthat::test_that(
  "resolve_region_event_predictor_specification() maps every region exactly",
  {
    expected_events <- list(
      "Asia" = c("fi", "fc", "ei"),
      "Europe" = c("fi", "fc", "ec", "cc"),
      "North America" = c("fc", "es"),
      "Latin America" = c("weak", "strong"),
      "Oceania" = c("weak", "medium", "strong")
    )
    expected_references <- c(
      "Asia" = "bi",
      "Europe" = "bi",
      "North America" = "bi",
      "Latin America" = "no_impact",
      "Oceania" = "no_impact"
    )

    purrr::iwalk(
      expected_events,
      function(events, region) {
        specification <- resolve_region_event_predictor_specification(
          region = region,
          proxy_variant = "events"
        )

        testthat::expect_identical(specification[["human"]], events)
        testthat::expect_identical(specification[["events"]], events)
        testthat::expect_identical(
          specification[["reference"]],
          unname(expected_references[[region]])
        )
        testthat::expect_false(
          specification[["reference"]] %in% specification[["human"]]
        )
      }
    )
  }
)

testthat::test_that(
  "resolve_region_event_predictor_specification() combines SPD and events",
  {
    specification <- resolve_region_event_predictor_specification(
      region = "Europe",
      proxy_variant = "spd_events"
    )

    testthat::expect_identical(
      specification[["human"]],
      c("spd", "fi", "fc", "ec", "cc")
    )
    testthat::expect_false("bi" %in% specification[["human"]])
  }
)

testthat::test_that(
  "resolve_region_event_predictor_specification() rejects invalid inputs",
  {
    testthat::expect_error(
      resolve_region_event_predictor_specification(region = "Atlantis"),
      regexp = "Unknown data region"
    )
    testthat::expect_error(
      resolve_region_event_predictor_specification(
        region = "Oceania",
        proxy_variant = "events",
        available_columns = c(
          "weak",
          "strong",
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        )
      ),
      regexp = "medium"
    )
  }
)
