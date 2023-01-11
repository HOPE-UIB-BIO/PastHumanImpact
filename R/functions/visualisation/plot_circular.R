plot_circular <- function(data_source,
                          y_var_name,
                          col_var_name = "predictor",
                          facet_var_name = NULL,
                          add_error = c(FALSE, "95%", "sd"),
                          add_polygon = c(FALSE, "mean", "95%", "sd"),
                          point_size = 7,
                          text_size = 12,
                          line_size = 0.1,
                          full_scale = FALSE) {
  y_var_name <- as.character(y_var_name)
  col_var_name <- as.character(col_var_name)
  add_error <- as.character(add_error)
  add_error <- match.arg(add_error)
  add_polygon <- as.character(add_polygon)
  add_polygon <- match.arg(add_polygon)
  full_scale <- as.character(full_scale)

  #  adjust `y_var_name` if has a `_mean`
  y_var_name_stripped <-
    stringr::str_replace(
      y_var_name, "_mean", ""
    )

  # make a plot
  p_theme <-
    data_source %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = get(y_var_name),
        x = predictor,
        fill = as.factor(get(col_var_name)),
        col = as.factor(get(col_var_name))
      )
    ) +
    ggplot2::theme_light() +
    ggplot2::coord_polar() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(linewidth = line_size),
      legend.position = "bottom",
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      y = y_var_name,
      fill = col_var_name,
      col = col_var_name
    )

  # add facet
  if (
    isFALSE(is.null(facet_var_name))
  ) {
    facet_var_name <- as.character(facet_var_name)

    p_0 <-
      p_theme +
      ggplot2::facet_wrap(
        as.formula(
          paste(
            "~",
            paste(facet_var_name, collapse = " + ")
          )
        )
      )
  } else {
    p_0 <-
      p_theme
  }

  # add scale
  switch(as.character(full_scale),
    "TRUE" = {
      max_value <- 1

      if (
        y_var_name_stripped == "i_perc_percent"
      ) {
        max_value <- 100
      }

      p_1 <-
        p_0 +
        ggplot2::scale_y_continuous(
          limits = c(0, max_value),
          breaks = seq(0, max_value, by = max_value / 10),
          minor_breaks = seq(0, max_value, by = max_value / 5)
        )
    },
    "FALSE" = {
      p_1 <- p_0
    }
  )

  # add polygon
  switch(as.character(add_polygon),
    "mean" = {
      p_2 <-
        p_1 +
        ggplot2::geom_polygon(
          mapping = ggplot2::aes(
            y = get(y_var_name),
            group = as.factor(get(col_var_name))
          ),
          alpha = 0.2,
          col = NA
        )
    },
    "95%" = {
      p_2 <-
        p_1 +
        ggplot2::geom_polygon(
          mapping = ggplot2::aes(
            y = get(paste0(y_var_name_stripped, "_upr")),
            group = as.factor(get(col_var_name))
          ),
          alpha = 0.2,
          col = NA
        )
    },
    "sd" = {
      p_2 <-
        p_1 +
        ggplot2::geom_polygon(
          mapping = ggplot2::aes(
            y = get(y_var_name) +
              get(paste0(y_var_name_stripped, "_sd")),
            group = as.factor(get(col_var_name))
          ),
          alpha = 0.2,
          col = NA
        )
    },
    "FALSE" = {
      p_2 <- p_1
    }
  )

  # add error bars
  switch(as.character(add_error),
    "FALSE" = {
      p_3 <-
        p_2
    },
    "95%" = {
      p_3 <-
        p_2 +
        ggplot2::geom_pointrange(
          mapping = ggplot2::aes(
            ymin = get(paste0(y_var_name_stripped, "_lwr")),
            ymax = get(paste0(y_var_name_stripped, "_upr"))
          ),
          fatten = 0,
          size = 3
        )
    },
    "sd" = {
      p_3 <-
        p_2 +
        ggplot2::geom_pointrange(
          mapping = ggplot2::aes(
            ymin = get(y_var_name) -
              get(paste0(y_var_name_stripped, "_sd")),
            ymax = get(y_var_name) +
              get(paste0(y_var_name_stripped, "_sd"))
          ),
          fatten = 0,
          size = 3
        )
    }
  )

  # add points
  if (
    point_size > 0
  ) {
    p_4 <-
      p_3 +
      ggplot2::geom_point(
        size = point_size,
        col = "gray30"
      ) +
      ggplot2::geom_point(
        size = point_size - 1
      )
  } else {
    p_4 <- p_3
  }

  return(p_4)
}
