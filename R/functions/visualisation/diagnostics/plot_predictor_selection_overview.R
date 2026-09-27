#' @title Plot local predictor retention across geographic groups
#' @description
#' Display the percentage of eligible filtered joint-human datasets retaining
#' each human and climate predictor by continental region and region.
#' @param selection_frequency_by_continental_region Predictor-retention
#' summary with a public `continental_region` field.
#' @param selection_frequency_by_region Predictor-retention summary with a
#' public `region` field.
#' @return A faceted heatmap showing retention rates and underlying dataset
#' counts.
#' @examples
#' \dontrun{
#' plot_predictor_selection_overview(by_continental_region, by_region)
#' }
plot_predictor_selection_overview <- function(
  selection_frequency_by_continental_region,
  selection_frequency_by_region
) {
  required <- c(
    "group", "predictor", "n_datasets", "n_selected", "retention_rate"
  )
  assertthat::assert_that(
    is.data.frame(selection_frequency_by_continental_region),
    is.data.frame(selection_frequency_by_region),
    "continental_region" %in%
      names(selection_frequency_by_continental_region),
    "region" %in% names(selection_frequency_by_region),
    all(required %in% names(selection_frequency_by_continental_region)),
    all(required %in% names(selection_frequency_by_region)),
    msg = "Predictor-selection plot inputs do not satisfy the contract."
  )

  continental_region_order <- names(region_labeller)
  continental_region_labels <-
    unname(region_labeller[continental_region_order])
  region_labels <- c(
    "POL", "CCS", "CWS", "CHS", "CDW", "CDS",
    "TMP", "TDW", "TDS", "TRO", "ARD"
  )
  raw_region_labels <- c(
    Polar = "POL",
    Cold_Without_dry_season_Cold_Summer = "CCS",
    Cold_Without_dry_season_Warm_Summer = "CWS",
    Cold_Without_dry_season_Hot_Summer = "CHS",
    Cold_Dry_Winter = "CDW",
    Cold_Dry_Summer = "CDS",
    Temperate_Without_dry_season = "TMP",
    Temperate_Dry_Winter = "TDW",
    Temperate_Dry_Summer = "TDS",
    Tropical = "TRO",
    Arid = "ARD"
  )
  predictor_labels <- c(
    spd_sqrt = "√SPD",
    kk10_fraction = "KK10",
    hyde_sqrt = "√HYDE",
    temp_annual = "Annual\ntemperature",
    temp_cold = "Cold-month\ntemperature",
    prec_summer = "Summer\nprecipitation",
    prec_win = "Winter\nprecipitation"
  )

  plot_data <- dplyr::bind_rows(
    selection_frequency_by_continental_region |>
      dplyr::transmute(
        geography = "Continental region",
        area = dplyr::recode(
          .data[["continental_region"]], !!!region_labeller
        ),
        group = .data[["group"]],
        predictor = .data[["predictor"]],
        n_datasets = .data[["n_datasets"]],
        n_selected = .data[["n_selected"]],
        retention_rate = .data[["retention_rate"]]
      ),
    selection_frequency_by_region |>
      dplyr::transmute(
        geography = "Region",
        area = dplyr::recode(
          .data[["region"]], !!!raw_region_labels
        ),
        group = .data[["group"]],
        predictor = .data[["predictor"]],
        n_datasets = .data[["n_datasets"]],
        n_selected = .data[["n_selected"]],
        retention_rate = .data[["retention_rate"]]
      )
  ) |>
    dplyr::mutate(
      geography = factor(
        .data[["geography"]],
        levels = c("Continental region", "Region")
      ),
      group = factor(
        stringr::str_to_title(.data[["group"]]),
        levels = c("Human", "Climate")
      ),
      predictor = factor(
        dplyr::recode(.data[["predictor"]], !!!predictor_labels),
        levels = unname(predictor_labels)
      ),
      area = factor(
        .data[["area"]],
        levels = rev(c(continental_region_labels, region_labels))
      ),
      cell_label = sprintf(
        "%.0f%%\n%s/%s", 100 * .data[["retention_rate"]],
        .data[["n_selected"]], .data[["n_datasets"]]
      )
    )

  res <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["predictor"]], y = .data[["area"]],
      fill = .data[["retention_rate"]]
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[["cell_label"]]),
      size = 2.65, lineheight = 0.9
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["geography"]]),
      cols = ggplot2::vars(.data[["group"]]),
      scales = "free", space = "free"
    ) +
    ggplot2::scale_fill_gradient(
      low = "white", high = "#4F646E", limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1),
      name = "Retained"
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Predictors retained after local collinearity filtering",
      subtitle = "Cells show retention percentage and selected/eligible sequence counts"
    ) +
    ggplot2::theme_bw(base_size = 10.5) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 0, hjust = 0.5, lineheight = 0.9
      ),
      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2", colour = common_gray
      ),
      strip.text.y = ggplot2::element_text(angle = 0),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
  return(res)
}
