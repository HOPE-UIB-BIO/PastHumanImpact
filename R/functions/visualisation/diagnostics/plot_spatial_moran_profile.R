#' @title Plot one spatial Moran diagnostic profile
#' @description
#' Show Moran's I before and after dbMEM filtering for the fixed-distance
#' global networks and continent-specific dbMEM connectivity networks.
#' @param data_moran Data frame of spatial Moran diagnostics.
#' @param profile Character scalar. Either `signed` or `zero_truncated`.
#' @return A `ggplot` object with one before-after panel per diagnostic unit.
#' @details
#' Filled points identify significant positive spatial autocorrelation using
#' the one-sided permutation result supplied in `positive_autocorrelation`.
#' @examples
#' \dontrun{
#' plot_spatial_moran_profile(
#'   data_moran = data_moran,
#'   profile = "signed"
#' )
#' }
plot_spatial_moran_profile <- function(
  data_moran,
  profile
) {
  required_columns <-
    c(
      "profile",
      "stage",
      "spatial_scope",
      "spatial_group",
      "distance_km",
      "moran_i",
      "positive_autocorrelation"
    )

  assertthat::assert_that(
    is.data.frame(data_moran),
    all(required_columns %in% names(data_moran)),
    is.character(profile),
    length(profile) == 1L,
    profile %in% c("signed", "zero_truncated"),
    msg = "Spatial Moran profile inputs do not satisfy the contract."
  )

  data_profile <-
    data_moran |>
    dplyr::filter(.data[["profile"]] == .env[["profile"]]) |>
    dplyr::mutate(
      stage_label = factor(
        .data[["stage"]],
        levels = c("unfiltered", "residual"),
        labels = c("Before", "After")
      ),
      region_label = dplyr::recode(
        .data[["spatial_group"]],
        !!!region_labeller
      ),
      diagnostic_label = dplyr::if_else(
        .data[["spatial_scope"]] == "global",
        stringr::str_glue(
          "All continents\n{round(.data[['distance_km']])} km"
        ),
        stringr::str_glue(
          "{.data[['region_label']]}\n",
          "connectivity {round(.data[['distance_km']])} km"
        )
      ),
      diagnostic_order = dplyr::case_when(
        .data[["spatial_scope"]] == "global" &
          .data[["distance_km"]] == 250 ~ 1,
        .data[["spatial_scope"]] == "global" &
          .data[["distance_km"]] == 500 ~ 2,
        .data[["spatial_group"]] == "North America" ~ 3,
        .data[["spatial_group"]] == "Latin America" ~ 4,
        .data[["spatial_group"]] == "Europe" ~ 5,
        .data[["spatial_group"]] == "Asia" ~ 6,
        .data[["spatial_group"]] == "Oceania" ~ 7,
        TRUE ~ 8
      )
    ) |>
    dplyr::arrange(
      .data[["diagnostic_order"]],
      .data[["stage_label"]]
    ) |>
    dplyr::mutate(
      diagnostic_label = factor(
        .data[["diagnostic_label"]],
        levels = unique(.data[["diagnostic_label"]])
      )
    )

  if (
    nrow(data_profile) == 0L
  ) {
    cli::cli_abort(
      "No Moran diagnostics are available for profile {.val {profile}}."
    )
  }

  res <-
    ggplot2::ggplot(
      data_profile,
      ggplot2::aes(
        x = .data[["stage_label"]],
        y = .data[["moran_i"]],
        group = .data[["diagnostic_label"]]
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["diagnostic_label"]]),
      ncol = 4
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        `FALSE` = "white",
        `TRUE` = "#6F5B8B"
      ),
      breaks = c(FALSE, TRUE),
      labels = c("Not detected", "Detected")
    ) +
    ggplot2::labs(
      x = "dbMEM filtering stage",
      y = "Moran's I\n(positive = nearby values are more similar)",
      fill = paste(
        "Positive spatial autocorrelation",
        "(one-sided permutation p < 0.05)"
      )
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = colorspace::lighten(common_gray, amount = 0.85),
        colour = common_gray
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = colorspace::lighten(common_gray, amount = 0.35),
      linewidth = line_size
    ) +
    ggplot2::geom_line(
      colour = common_gray,
      linewidth = line_size * 1.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data[["positive_autocorrelation"]]),
      shape = 21,
      colour = common_gray,
      size = point_size * 1.5,
      stroke = line_size
    )

  return(res)
}
