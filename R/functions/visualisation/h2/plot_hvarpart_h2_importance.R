#' @title Plot an H2 HVarPart inset
#' @description
#' Draws either a zero-truncated human-minus-climate balance lollipop or an
#' unstacked signed supplementary inset for one region and climate zone.
#' @param data_summary Pooled H2 importance summary.
#' @param selected_region Region to display.
#' @param selected_climatezone Climate zone to display.
#' @param profile One of `"zero_truncated"` or `"signed"`.
#' @param legend_position ggplot2 legend position.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' plot_hvarpart_h2_importance(
#'   data_summary = h2_summary,
#'   selected_region = "Europe",
#'   selected_climatezone = "Polar",
#'   profile = "zero_truncated"
#' )
#' }
plot_hvarpart_h2_importance <- function(
  data_summary,
  selected_region,
  selected_climatezone,
  profile = c("zero_truncated", "signed"),
  legend_position = "none"
) {
  profile <- match.arg(profile)
  required <- c(
    "region", "climatezone", "predictor", "pooled_allocation"
  )
  assertthat::assert_that(
    is.data.frame(data_summary),
    all(required %in% names(data_summary)),
    assertthat::is.string(selected_region),
    assertthat::is.string(selected_climatezone),
    msg = "H2 importance plot inputs do not satisfy the required contract."
  )

  data_plot <-
    data_summary |>
    dplyr::filter(
      .data[["region"]] == selected_region,
      .data[["climatezone"]] == selected_climatezone
    ) |>
    dplyr::mutate(
      predictor = factor(
        .data[["predictor"]],
        levels = c("climate", "human")
      )
    )

  assertthat::assert_that(
    nrow(data_plot) %in% c(0L, 2L),
    msg = "Selected H2 stratum must contain both predictors or no data."
  )

  if (nrow(data_plot) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          )
        )
    )
  }

  if (identical(profile, "zero_truncated")) {
    data_balance <-
      compute_hvarpart_importance_balance(
        data_summary = data_plot,
        group_vars = c("region", "climatezone")
      ) |>
      dplyr::mutate(
        x_position = 1
      )
    background_values <- seq(-1, 1, length.out = 201)
    background_step <- background_values[[2]] - background_values[[1]]
    data_background <-
      tibble::tibble(
        background_balance = background_values,
        ymin = background_values - background_step / 2,
        ymax = background_values + background_step / 2
      )

    result <-
      ggplot2::ggplot(data_balance) +
      ggplot2::geom_rect(
        data = data_background,
        mapping = ggplot2::aes(
          xmin = 0.5,
          xmax = 1.5,
          ymin = .data[["ymin"]],
          ymax = .data[["ymax"]],
          fill = .data[["background_balance"]]
        ),
        colour = NA,
        alpha = 0.25,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = common_gray,
        linewidth = line_size * 3,
        linetype = 2
      ) +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = .data[["x_position"]],
          xend = .data[["x_position"]],
          y = 0,
          yend = .data[["importance_balance"]]
        ),
        colour = colorspace::lighten(common_gray, amount = 0.25),
        linewidth = line_size * 3
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          x = .data[["x_position"]],
          y = .data[["importance_balance"]],
          fill = .data[["importance_balance"]]
        ),
        shape = 21,
        colour = common_gray,
        stroke = line_size * 7,
        size = point_size * 4.5
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0.2, 1.8),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-1.3, 1.3),
        breaks = c(-1, 0, 1),
        labels = c("Climate impact", "Equal", "Human impact"),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_fill_gradient2(
        low = palette_predictors[["climate"]],
        mid = "#F2F2F2",
        high = palette_predictors[["human"]],
        midpoint = 0,
        limits = c(-1, 1),
        oob = scales::squish,
        guide = "none"
      )
  } else {
    result <-
      ggplot2::ggplot(
        data_plot,
        ggplot2::aes(
          x = .data[["climatezone"]],
          y = .data[["pooled_allocation"]],
          fill = .data[["predictor"]]
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 1,
        colour = "grey75",
        linewidth = line_size
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = "grey75",
        linewidth = line_size,
        linetype = 2
      ) +
      ggplot2::geom_col(
        width = 0.7,
        position = ggplot2::position_dodge(width = 0.75),
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        "Predictors",
        values = palette_predictors,
        drop = FALSE,
        guide = ggplot2::guide_legend(
          title.position = "top",
          nrow = 2,
          byrow = TRUE
        )
      )
  }

  result <-
    result +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_text(size = text_size),
      legend.text = ggplot2::element_text(size = text_size),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    )

  if (identical(profile, "zero_truncated")) {
    result <-
      result +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(
          fill = "white",
          colour = NA
        ),
        panel.border = ggplot2::element_rect(
          fill = NA,
          colour = common_gray,
          linewidth = line_size * 2
        )
      )
  }

  return(result)
}
