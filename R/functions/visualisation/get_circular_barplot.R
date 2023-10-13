get_circular_barplot <- function(data_source,
                                 y_var = "percentage_median",
                                 x_var = "predictor",
                                 fill_var = "sel_classification",
                                 col_vec = palette_ecozones,
                                 x_name = x_label,
                                 y_step = 10,
                                 y_max = 50,
                                 line_col = "grey60",
                                 line_width = 0.1,
                                 background_col = "transparent",
                                 text_size = 6,
                                 icon_size = 0.2) {
  data_source_trunk <-
    data_source %>%
    dplyr::mutate(
      y_var_large = ifelse(get(y_var) > y_max, TRUE, FALSE),
      !!y_var := ifelse(get(y_var) > y_max, y_max, get(y_var)),
    ) %>%
    dplyr::mutate(
      !!x_var := factor(
        get(x_var),
        levels = c(
          "human",
          "time",
          "climate"
        )
      ),
      !!x_var := forcats::fct_recode(
        get(x_var),
        "Human" = "human",
        "Time" = "time",
        "Climate" = "climate"
      )
    )

  circular_p <-
    data_source_trunk %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = get(x_var),
        y = get(y_var)
      )
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = c(-5, y_max + 0.1),
      breaks = seq(0, y_max, by = y_step)
    ) +
    ggplot2::scale_x_discrete(
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = col_vec,
      drop = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = col_vec,
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
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.spacing = grid::unit(c(0, 0, 0, 0), "mm")
    ) +
    # add lines for every 10 percent
    ggplot2::geom_hline(
      yintercept = seq(0, y_max, by = y_step),
      color = line_col,
      linewidth = line_width
    ) +
    ggplot2::geom_vline(
      xintercept = (0:3) + 0.5,
      linetype = "dashed",
      linewidth = line_width,
      color = line_col
    ) +
    ggplot2::annotate(
      "text",
      x = rep(3.5, length(seq(20, y_max, by = y_step))),
      y = seq(20, y_max, by = y_step),
      label = paste0(seq(20, y_max, by = y_step), " %"),
      vjust = 0,
      size = 1.5
    ) +
    ggplot2::geom_col(
      data = data_source_trunk %>%
        dplyr::filter(
          grepl(
            "unique_percent",
            variance_partition
          )
        ),
      mapping = ggplot2::aes(
        fill = get(fill_var)
      ),
      width = 0.6,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      alpha = 1
    ) +
    ggplot2::geom_col(
      data = data_source_trunk %>%
        dplyr::filter(grepl(
          "average_share_percent",
          variance_partition
        )),
      mapping = ggplot2::aes(
        fill = get(fill_var)
      ),
      width = .6,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      alpha = 0.4
    ) +
    ggimage::geom_image(
      data = data.frame(
        x = c(0.75, 2, 3.25),
        y = rep(y_max, 3),
        image = c(
          here::here(
            "Outputs/icons/human.png"
          ),
          here::here(
            "Outputs/icons/time.png"
          ),
          here::here(
            "Outputs/icons/climate.png"
          )
        )
      ),
      mapping = ggplot2::aes(
        x = x,
        y = y,
        image = image
      ),
      size = icon_size,
      alpha = 1
    ) +
    ggplot2::coord_polar()

  return(circular_p)
}
