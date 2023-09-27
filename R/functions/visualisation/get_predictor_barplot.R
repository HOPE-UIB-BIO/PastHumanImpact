# plot predictor bars of temporal analysis

get_predictor_barplot <-
  function(data,
           sel_predictor = "human",
           x_var = "percentage_median",
           axis_to_right = TRUE,
           sel_palette) {
    p_predictor <-
      data %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        data = data %>%
          dplyr::filter(
            variance_partition == "Unique_percent_median",
            predictor %in% sel_predictor
          ),
        ggplot2::aes(
          y = as.factor(age / 1000),
          x = get(x_var),
          fill = predictor
        ),
        stat = "identity",
        width = .6,
        alpha = 1,
        show.legend = FALSE
      ) +
      ggplot2::geom_bar(
        data = data %>%
          dplyr::filter(
            variance_partition == "Average.share_percent_median",
            predictor %in% sel_predictor
          ),
        ggplot2::aes(
          y = as.factor(age / 1000),
          x = get(x_var),
          fill = predictor
        ),
        stat = "identity",
        width = .6,
        alpha = 0.4,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = sel_palette,
        drop = FALSE
      ) +
      ggplot2::scale_x_continuous(
        name = NULL,
        limits = c(0, 100),
        breaks = seq(0, 100, by = 25),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_discrete(
        limits = rev,
        drop = FALSE
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(
          fill = "transparent", color = NA
        ),
        plot.background = ggplot2::element_rect(
          fill = "transparent", color = NA
        ),
        line = ggplot2::element_line(linewidth = 0.01),
        text = ggplot2::element_text(size = 8, color = "grey30"),
        axis.text.x = ggplot2::element_text(size = 8, angle = 60),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
      ) +
      ggplot2::labs(
        y = "",
        x = ""
      )


    if (isTRUE(axis_to_right)) {
      p_predictor <-
        p_predictor +
        ggplot2::scale_x_reverse(
          name = NULL,
          limits = c(100, 0),
          breaks = seq(0, 100, by = 20),
          expand = c(0, 0)
        ) +
        ggplot2::scale_y_discrete(
          limits = rev,
          position = "right",
          drop = FALSE
        )
    } else {
      p_predictor
    }


    p_predictor
  }
