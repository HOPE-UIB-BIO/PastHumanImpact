#' @title Build a labelled H2 HVarPart figure
#' @description
#' Arranges region-by-climate-zone panels with row and column labels, then adds
#' a framed guide row for importance, predictors, age, and ordination.
#' @param plot_list List of cell plots ordered by region and then climate zone.
#' @param region_labels Character vector of region labels.
#' @param climatezone_labels Character vector of climate-zone labels.
#' @param climatezone_colours Character vector of climate-zone band colours.
#' @param importance_guide Plot or grob explaining the importance inset.
#' @param importance_title Character heading for the importance guide.
#' @param predictor_legend Plot or grob containing the predictor legend.
#' @param age_legend Plot or grob containing the age legend.
#' @param trajectory_guide Plot or grob explaining the ordination trajectories.
#' @return A composite ggplot object.
#' @examples
#' \dontrun{
#' result <- build_composite_hvarpart_h2_figure(
#'   plot_list = rep(list(ggplot2::ggplot()), 4),
#'   region_labels = c("Region A", "Region B"),
#'   climatezone_labels = c("Zone A", "Zone B"),
#'   climatezone_colours = c("#8C4418", "#562FB1"),
#'   importance_guide = ggplot2::ggplot(),
#'   importance_title = "Importance",
#'   predictor_legend = ggplot2::ggplot(),
#'   age_legend = ggplot2::ggplot(),
#'   trajectory_guide = ggplot2::ggplot()
#' )
#' }
build_composite_hvarpart_h2_figure <- function(
  plot_list,
  region_labels,
  climatezone_labels,
  climatezone_colours,
  importance_guide,
  importance_title,
  predictor_legend,
  age_legend,
  trajectory_guide
) {
  assertthat::assert_that(
    is.list(plot_list),
    is.character(region_labels),
    length(region_labels) > 0L,
    is.character(climatezone_labels),
    length(climatezone_labels) > 0L,
    is.character(climatezone_colours),
    length(climatezone_colours) == length(climatezone_labels),
    all(!is.na(climatezone_colours)),
    assertthat::is.string(importance_title),
    length(plot_list) ==
      length(region_labels) * length(climatezone_labels),
    msg = "H2 composite plots and labels do not form a complete grid."
  )

  make_label_plot <- function(label, angle = 0, label_size = text_size) {
    result <-
      cowplot::ggdraw() +
      cowplot::draw_label(
        label = label,
        x = 0.5,
        y = 0.5,
        hjust = 0.5,
        vjust = 0.5,
        angle = angle,
        colour = common_gray,
        size = label_size
      )

    return(result)
  }

  make_climatezone_header <- function(label, colour) {
    wrapped_label <-
      label |>
      stringr::str_replace_all(" - ", " ") |>
      stringr::str_replace_all(" ", "\n")
    result <-
      ggplot2::ggplot() +
      ggplot2::annotate(
        geom = "rect",
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 0.14,
        fill = colour,
        colour = NA
      ) +
      ggplot2::annotate(
        geom = "text",
        x = 0.5,
        y = 0.6,
        label = wrapped_label,
        colour = common_gray,
        size = text_size * 0.85 / ggplot2::.pt
      ) +
      ggplot2::coord_cartesian(
        xlim = c(0, 1),
        ylim = c(0, 1),
        expand = FALSE,
        clip = "off"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
      )

    return(result)
  }

  climatezone_header_plots <-
    purrr::map2(
      climatezone_labels,
      climatezone_colours,
      .f = ~ make_climatezone_header(
        label = .x,
        colour = .y
      )
    )
  climatezone_header <-
    cowplot::plot_grid(
      plotlist = climatezone_header_plots,
      nrow = 1
    )
  header_row <-
    cowplot::plot_grid(
      NULL,
      climatezone_header,
      nrow = 1,
      rel_widths = c(0.65, length(climatezone_labels))
    )

  body_rows <-
    seq_along(region_labels) |>
    purrr::map(
      .f = ~ {
        first_index <-
          (.x - 1L) * length(climatezone_labels) + 1L
        last_index <-
          first_index + length(climatezone_labels) - 1L
        cell_grid <-
          cowplot::plot_grid(
            plotlist = plot_list[first_index:last_index],
            nrow = 1
          )

        return(
          cowplot::plot_grid(
            make_label_plot(
              label = region_labels[[.x]],
              angle = 90,
              label_size = text_size * 0.75
            ),
            cell_grid,
            nrow = 1,
            rel_widths = c(0.65, length(climatezone_labels))
          )
        )
      }
    )
  labelled_grid <-
    cowplot::plot_grid(
      header_row,
      cowplot::plot_grid(plotlist = body_rows, ncol = 1),
      ncol = 1,
      rel_heights = c(0.9, length(region_labels))
    )

  trajectory_panel <-
    cowplot::ggdraw() +
    cowplot::draw_label(
      label = "Ordination",
      x = 0.5,
      y = 0.98,
      hjust = 0.5,
      vjust = 1,
      colour = common_gray,
      size = text_size
    ) +
    cowplot::draw_plot(
      plot = trajectory_guide,
      x = 0,
      y = 0,
      width = 1,
      height = 0.9
    )
  importance_panel <-
    cowplot::ggdraw() +
    cowplot::draw_label(
      label = importance_title,
      x = 0.5,
      y = 0.98,
      hjust = 0.5,
      vjust = 1,
      colour = common_gray,
      size = text_size
    ) +
    cowplot::draw_plot(
      plot = importance_guide,
      x = 0,
      y = 0,
      width = 1,
      height = 0.9
    )
  guide_row <-
    cowplot::plot_grid(
      importance_panel,
      predictor_legend,
      NULL,
      age_legend,
      NULL,
      trajectory_panel,
      nrow = 1,
      rel_widths = c(1.45, 1.05, 0.15, 1.3, 0.15, 1.4)
    )
  framed_guides <-
    cowplot::ggdraw() +
    ggplot2::annotate(
      geom = "rect",
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      fill = NA,
      colour = colorspace::lighten(common_gray, amount = 0.55),
      linewidth = line_size * 2
    ) +
    cowplot::draw_plot(
      plot = guide_row,
      x = 0.01,
      y = 0.02,
      width = 0.98,
      height = 0.96
    )

  result <-
    cowplot::plot_grid(
      labelled_grid,
      framed_guides,
      ncol = 1,
      rel_heights = c(length(region_labels) + 0.9, 2)
    )

  return(result)
}
