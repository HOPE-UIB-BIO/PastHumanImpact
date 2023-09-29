get_circular_barplot <- function(data_source,
                                 y_var = "percentage_median",
                                 x_var = "predictor",
                                 fill_var = "sel_classification",
                                 col_vec = palette_ecozones,
                                 x_name = x_label,
                                 y_step = 10,
                                 line_col = "grey60",
                                 line_width = 0.1,
                                 background_col = "transparent",
                                 text_size = 6) {
  circular_p <-
    data_source %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = get(x_var),
        y = get(y_var),
        fill = get(fill_var)
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-5, 40.1),
      expand = c(0, 0),
      breaks = seq(0, 50, by = y_step)
    ) +
    ggplot2::scale_x_discrete(
      label = x_name,
      drop = FALSE
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(
        fill = background_col, color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = background_col, color = NA
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = line_col,
        linewidth = line_width
      ),
      panel.grid.major.x = ggplot2::element_line(
        color = line_col,
        linetype = "dashed",
        linewidth = line_width
      ),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = text_size,
        vjust = -3
      ),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.spacing = grid::unit(c(0, 0, 0, 0), "mm")
    ) +
    ggplot2::scale_fill_manual(
      values = col_vec,
      drop = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = c(3.5, 3.5, 3.5),
      y = seq(20, 40, by = y_step),
      label = paste0(seq(20, 40, by = y_step), " %"),
      vjust = 0,
      size = 1.5
    ) +
    # add lines for every 10 percent
    # ggplot2::geom_hline(
    #  yintercept = seq(0, 50, by = y_step),
    #  color = "grey90"
    # ) +
    #    ggplot2::geom_vline(
    #  xintercept = (0:3) + 0.5,
    #  linetype = "dashed",
    #  linewidth = 0.01,
    #  color = "grey90"
    # ) +
    ggplot2::geom_col(
      data = data_source %>%
        dplyr::filter(
          grepl(
            "unique_percent",
            variance_partition
          )
        ),
      width = 0.6,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      alpha = 1
    ) +
    ggplot2::geom_col(
      data = data_source %>%
        dplyr::filter(grepl(
          "average_share_percent",
          variance_partition
        )),
      width = .6,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      alpha = 0.4
    ) +
    ggplot2::coord_polar()

  return(circular_p)
}
