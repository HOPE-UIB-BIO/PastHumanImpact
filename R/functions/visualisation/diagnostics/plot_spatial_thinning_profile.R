#' @title Plot one spatial thinning sensitivity profile
#' @description
#' Show distributions of pooled human-climate results across spatial thinning
#' repetitions, faceted by the requested minimum distance.
#' @param data_sensitivity Spatial thinning and unthinned summary table.
#' @param data_estimates Spatially filtered aggregation estimates.
#' @param profile Character scalar. Either `signed` or `zero_truncated`.
#' @return A `ggplot` object with one panel per thinning distance.
#' @details
#' Reference lines show the unthinned pooled estimate and the spatially
#' filtered estimate at dbMEM equal to zero.
#' @examples
#' \dontrun{
#' plot_spatial_thinning_profile(
#'   data_sensitivity = data_sensitivity,
#'   data_estimates = data_estimates,
#'   profile = "signed"
#' )
#' }
plot_spatial_thinning_profile <- function(
  data_sensitivity,
  data_estimates,
  profile
) {
  required_sensitivity <-
    c(
      "sensitivity_type",
      "aggregation_level",
      "profile",
      "importance_balance",
      "distance_km"
    )

  required_estimates <-
    c(
      "aggregation_level",
      "profile",
      "adjusted_balance"
    )

  assertthat::assert_that(
    is.data.frame(data_sensitivity),
    all(required_sensitivity %in% names(data_sensitivity)),
    is.data.frame(data_estimates),
    all(required_estimates %in% names(data_estimates)),
    is.character(profile),
    length(profile) == 1L,
    profile %in% c("signed", "zero_truncated"),
    msg = "Spatial thinning profile inputs do not satisfy the contract."
  )

  data_thinning <-
    data_sensitivity |>
    dplyr::filter(
      .data[["sensitivity_type"]] == "thinning",
      .data[["aggregation_level"]] == "overall",
      .data[["profile"]] == .env[["profile"]]
    ) |>
    dplyr::mutate(
      distance_label = factor(
        stringr::str_glue(
          "Nearby records removed within {.data[['distance_km']]} km"
        ),
        levels = c(
          "Nearby records removed within 250 km",
          "Nearby records removed within 500 km"
        )
      )
    )

  data_reference <-
    dplyr::bind_rows(
      data_sensitivity |>
        dplyr::filter(
          .data[["sensitivity_type"]] == "unthinned",
          .data[["aggregation_level"]] == "overall",
          .data[["profile"]] == .env[["profile"]]
        ) |>
        dplyr::transmute(
          reference = "All eligible SPD records",
          value = .data[["importance_balance"]]
        ),
      data_estimates |>
        dplyr::filter(
          .data[["aggregation_level"]] == "overall",
          .data[["profile"]] == .env[["profile"]]
        ) |>
        dplyr::transmute(
          reference = "After accounting for spatial structure",
          value = .data[["adjusted_balance"]]
        )
    ) |>
    dplyr::mutate(
      reference = factor(
        .data[["reference"]],
        levels = c(
          "All eligible SPD records",
          "After accounting for spatial structure"
        )
      )
    )

  if (
    nrow(data_thinning) == 0L || nrow(data_reference) != 2L
  ) {
    cli::cli_abort(
      "Incomplete thinning results are available for profile {.val {profile}}."
    )
  }

  x_axis_label <-
    dplyr::case_when(
      profile == "signed" ~ paste(
        "SPD human-climate relative importance",
        "(Human contribution minus climate contribution)",
        sep = "\n"
      ),
      TRUE ~ paste(
        "SPD human-climate relative importance",
        "(Human share minus climate share)",
        sep = "\n"
      )
    )

  res <-
    ggplot2::ggplot(
      data_thinning,
      ggplot2::aes(x = .data[["importance_balance"]])
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["distance_label"]]),
      ncol = 1,
      scales = "free_x"
    ) +
    ggplot2::scale_x_continuous(n.breaks = 5) +
    ggplot2::scale_colour_manual(
      values = c(
        "All eligible SPD records" = common_gray,
        "After accounting for spatial structure" = "#6F5B8B"
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "All eligible SPD records" = "dashed",
        "After accounting for spatial structure" = "solid"
      )
    ) +
    ggplot2::labs(
      x = x_axis_label,
      y = "Distribution across spatially thinned datasets\n(100 repetitions)",
      colour = NULL,
      linetype = NULL,
      caption = paste(
        "Each value compares human and climate influence in one thinned",
        "SPD dataset. Negative values indicate greater climate influence."
      )
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = colorspace::lighten(common_gray, amount = 0.85),
        colour = common_gray
      )
    ) +
    ggplot2::geom_density(
      fill = colorspace::lighten(common_gray, amount = 0.7),
      colour = common_gray,
      linewidth = line_size * 1.5
    ) +
    ggplot2::geom_vline(
      data = data_reference,
      ggplot2::aes(
        xintercept = .data[["value"]],
        colour = .data[["reference"]],
        linetype = .data[["reference"]]
      ),
      linewidth = line_size * 1.5,
      show.legend = TRUE
    )

  return(res)
}
