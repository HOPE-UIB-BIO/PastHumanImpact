#' @title Plot spatiotemporally controlled spatial analysis
#' @description
#' Insert time-controlled dataset balances and spatially adjusted summaries
#' into the canonical spatial maps, densities, and climate-zone panels.
#' @param data_records Eligible time-controlled dataset records.
#' @param data_estimates Spatially adjusted aggregation estimates.
#' @param data_geo_koppen Spatial climate-zone data used by
#'   `build_region_map()`.
#' @param profile Balance profile. Use `"zero_truncated"` for the bounded
#' presentation balance or `"signed"` for the untruncated hierarchical
#' hierarchical contribution difference on the adjusted-R-squared scale.
#' @return A ggplot object containing maps and statistical panels.
#' @examples
#' \dontrun{
#' plot_h1_spatial_controlled_balance(records, estimates, koppen)
#' }
plot_h1_spatial_controlled_balance <- function(
  data_records,
  data_estimates,
  data_geo_koppen,
  profile = c("zero_truncated", "signed")
) {
  profile <-
    match.arg(profile)

  balance_column <-
    if (
      identical(profile, "zero_truncated")
    ) {
      "zero_balance"
    } else {
      "signed_difference"
    }

  y_axis_title <-
    if (
      identical(profile, "zero_truncated")
    ) {
      paste(
        "Zero-truncated human-climate importance balance",
        "(human share \u2212 climate share)",
        sep = "\n"
      )
    } else {
      paste(
        "Untruncated signed human-climate hierarchical contribution",
        "(adjusted-R-squared scale)",
        sep = "\n"
      )
    }

  required_records <-
    c(
      "dataset_id",
      "analysis",
      "region",
      "climatezone",
      "long",
      "lat",
      balance_column
    )

  required_estimates <-
    c(
      "aggregation_level",
      "profile",
      "region",
      "climatezone",
      "adjusted_balance"
    )

  assertthat::assert_that(
    is.data.frame(data_records),
    all(required_records %in% names(data_records)),
    is.data.frame(data_estimates),
    all(required_estimates %in% names(data_estimates)),
    is.data.frame(data_geo_koppen),
    all(c("x", "y", "climatezone") %in% names(data_geo_koppen)),
    all(is.finite(data_records[[balance_column]])),
    all(abs(data_records[[balance_column]]) <= 1 + 1e-10),
    msg = paste(
      "Spatiotemporally controlled spatial analysis inputs do not",
      "satisfy the contract."
    )
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")

  balance_palette <-
    c(
      palette_predictors[["climate"]],
      "#F2F2F2",
      palette_predictors[["human"]]
    )

  balance_colour <-
    scales::col_numeric(
      palette = balance_palette,
      domain = c(-1, 1)
    )

  data_balance_records <-
    data_records |>
    dplyr::filter(is.finite(.data[[balance_column]])) |>
    dplyr::mutate(
      model_id = stringr::str_c(
        .data[["analysis"]],
        .data[["dataset_id"]],
        sep = "__"
      ),
      importance_balance = .data[[balance_column]],
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    prepare_climatezone_factor()

  data_importance_stub <-
    data_balance_records |>
    dplyr::select(
      dplyr::all_of(
        c(
          "model_id",
          "dataset_id",
          "region",
          "climatezone",
          "importance_balance"
        )
      )
    ) |>
    tidyr::crossing(predictor = c("human", "climate")) |>
    dplyr::mutate(
      analysis = "spatial_spd",
      individual = dplyr::if_else(
        .data[["predictor"]] == "human",
        (.data[["importance_balance"]] + 1) / 2,
        (1 - .data[["importance_balance"]]) / 2
      ),
      total_adjusted_r_squared = 1,
      has_negative_individual = FALSE,
      is_importance_eligible = TRUE
    )

  data_zone_summary <-
    select_spatial_balance_estimates(
      data_estimates = data_estimates,
      aggregation_level = "region_climatezone",
      profile = profile
    ) |>
    dplyr::transmute(
      analysis = "spatial_spd",
      region = factor(.data[["region"]], levels = region_levels),
      climatezone = .data[["climatezone"]],
      importance_balance = .data[["adjusted_balance"]]
    ) |>
    prepare_climatezone_factor() |>
    dplyr::mutate(
      climate_colour = unname(
        palette_ecozones[as.character(.data[["climatezone"]])]
      )
    )
  data_region_summary <-
    select_spatial_balance_estimates(
      data_estimates = data_estimates,
      aggregation_level = "region",
      profile = profile
    ) |>
    dplyr::transmute(
      analysis = "spatial_spd",
      region = factor(.data[["region"]], levels = region_levels),
      importance_balance = .data[["adjusted_balance"]],
      line_colour = balance_colour(.data[["adjusted_balance"]])
    )
  data_meta <-
    data_balance_records |>
    dplyr::distinct(
      .data[["dataset_id"]],
      .data[["long"]],
      .data[["lat"]],
      .data[["region"]],
      .data[["climatezone"]]
    )
  result <-
    plot_hvarpart_spatial_balance(
      data_importance = data_importance_stub,
      data_meta = data_meta,
      data_geo_koppen = data_geo_koppen,
      data_records_override = data_balance_records,
      data_climatezone_summary_override = data_zone_summary,
      data_region_summary_override = data_region_summary,
      show_intervals = TRUE,
      y_axis_title = y_axis_title
    )

  res <-
    result[["plot"]]

  return(res)
}
