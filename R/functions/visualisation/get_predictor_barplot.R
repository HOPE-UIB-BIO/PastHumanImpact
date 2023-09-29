# plot predictor bars of temporal analysis

get_predictor_barplot <-
  function(data_source,
           sel_predictor = "human",
           x_var = "percentage_median",
           axis_to_right = TRUE,
           sel_palette,
           time_step = 1,
           text_size = 6) {
    data_age_dummy <-
      data_source %>%
      dplyr::filter(predictor == sel_predictor) %>%
      dplyr::distinct(age, predictor)

    p_predictor <-
      tibble::tibble() %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        data = data_age_dummy,
        mapping = ggplot2::aes(
          y = as.factor(age / 1000),
          x = 100
        ),
        fill = "gray80",
        stat = "identity",
        width = 0.6,
        alpha = 1,
        show.legend = FALSE
      ) +
      ggplot2::geom_bar(
        data = data_source %>%
          dplyr::filter(
            variance_partition == "unique_percent_median",
            predictor %in% sel_predictor
          ),
        mapping = ggplot2::aes(
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
        data = data_source %>%
          dplyr::filter(
            variance_partition == "average_share_percent_median",
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
        drop = FALSE,
        breaks = as.factor(seq(0, 10, time_step))
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(
          fill = "transparent", color = NA
        ),
        plot.background = ggplot2::element_rect(
          fill = "transparent", color = NA
        ),
        line = ggplot2::element_line(linewidth = 0.01),
        text = ggplot2::element_text(size = text_size, color = "grey30"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank()
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
          drop = FALSE,
          breaks = as.factor(seq(0, 10, time_step))
        )
    } else {
      p_predictor
    }


    p_predictor
  }
