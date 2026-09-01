#' Resolve region-specific human and climate predictors
#'
#' @param region Character scalar identifying the data region.
#' @param proxy_variant Character scalar. One of `"spd"`, `"spd_events"`, or
#'   `"events"`.
#' @param climate_predictors Character vector of climate predictors shared by
#'   all regions and proxy variants.
#' @param available_columns Optional character vector of columns available to
#'   the model. Required regional predictors are checked when supplied.
#'
#' @return A named list containing the region, proxy variant, human and climate
#'   predictors, active event predictors, reference category, and excluded
#'   event variables.
#'
#' @export
resolve_region_event_predictor_specification <-
  function(
    region,
    proxy_variant = c("spd", "spd_events", "events"),
    climate_predictors = c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    ),
    available_columns = NULL
  ) {
    proxy_variant <- base::match.arg(proxy_variant)

    event_predictors <- list(
      "Asia" = c("fi", "fc", "ei"),
      "Europe" = c("fi", "fc", "ec", "cc"),
      "North America" = c("fc", "es"),
      "Latin America" = c("weak", "strong"),
      "Oceania" = c("weak", "medium", "strong")
    )
    reference_categories <- c(
      "Asia" = "bi",
      "Europe" = "bi",
      "North America" = "bi",
      "Latin America" = "no_impact",
      "Oceania" = "no_impact"
    )

    if (length(region) != 1L || is.na(region) ||
        !region %in% names(event_predictors)) {
      cli::cli_abort(
        c(
          "Unknown data region {.val {region}}.",
          "i" = "Expected one of: {.val {names(event_predictors)}}."
        )
      )
    }

    region_events <- event_predictors[[region]]
    reference_category <- reference_categories[[region]]
    all_event_predictors <- unique(c(
      unlist(event_predictors, use.names = FALSE),
      unname(reference_categories)
    ))

    human_predictors <- switch(
      proxy_variant,
      "spd" = "spd",
      "spd_events" = c("spd", region_events),
      "events" = region_events
    )

    requested_predictors <- c(human_predictors, climate_predictors)
    if (!is.null(available_columns)) {
      missing_predictors <- setdiff(requested_predictors, available_columns)
      if (length(missing_predictors) > 0L) {
        cli::cli_abort(
          c(
            "Required predictors are missing for region {.val {region}}.",
            "x" = "Missing: {.val {missing_predictors}}."
          )
        )
      }
    }

    list(
      region = region,
      proxy_variant = proxy_variant,
      human = human_predictors,
      climate = climate_predictors,
      events = if (proxy_variant == "spd") character() else region_events,
      reference = reference_category,
      excluded_events = setdiff(all_event_predictors, region_events)
    )
  }
