plot_circular <- function(data_source,
                          y_var_name,
                          col_var_name,
                          facet_var_name = NULL,
                          add_error = c(FALSE, "95%", "sd"),
                          add_polygon = c(FALSE, "mean", "95%", "sd"),
                          full_scale = FALSE) {
  y_var_name <- as.character(y_var_name)
  col_var_name <- as.character(col_var_name)
  add_error <- as.character(add_error)
  add_error <- match.arg(add_error)
  add_polygon <- as.character(add_polygon)
  add_polygon <- match.arg(add_polygon)
  full_scale <- as.character(full_scale)

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
        y_var_name == "i_perc_percent"
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
            y = get(paste0(y_var_name, "_upr")),
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
              get(paste0(y_var_name, "_sd")),
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
            ymin = get(paste0(y_var_name, "_lwr")),
            ymax = get(paste0(y_var_name, "_upr"))
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
              get(paste0(y_var_name, "_sd")),
            ymax = get(y_var_name) +
              get(paste0(y_var_name, "_sd"))
          ),
          fatten = 0,
          size = 3
        )
    }
  )

  p_4 <-
    p_3 +
    ggplot2::geom_point(
      size = 7,
      col = "gray30"
    ) +
    ggplot2::geom_point(
      size = 6
    )

  return(p_4)
}
