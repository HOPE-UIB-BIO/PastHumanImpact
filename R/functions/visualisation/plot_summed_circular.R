plot_summed_circular <- function(data_source,
                                 group_vars = c(
                                   "region",
                                   "ecozone_koppen_5"
                                 ),
                                 col_var = "predictor",
                                 sel_mode = c(
                                   "individual",
                                   "unique",
                                   "average_share",
                                   "i_perc_percent"
                                 ),
                                 full_scale = FALSE) {
  sel_mode <- match.arg(sel_mode)

  # helper function
  # summarise adj_r2 across `group_vars`
  get_r2_summary_varpar <- function(data_source,
                                    group_vars = c(
                                      "region",
                                      "ecozone_koppen_5"
                                    )) {
    data_meta_sum <-
      data_source %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vars)
        )
      ) %>%
      dplyr::summarise(
        .groups = "drop",
        dplyr::across(
          c("long", "lat"),
          list(
            mean = ~ mean(.x, na.rm = TRUE)
          )
        )
      )

    data_r2_sum <-
      data_source %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vars)
        )
      ) %>%
      dplyr::summarise(
        .groups = "drop",
        dplyr::across(
          c("unique", "individual", "average_share", "i_perc_percent"),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            upr = ~ stats::quantile(.x, 0.975, na.rm = TRUE),
            lwr = ~ stats::quantile(.x, 0.025, na.rm = TRUE)
          )
        )
      )

    dplyr::left_join(
      data_meta_sum,
      data_r2_sum,
      by = group_vars
    ) %>%
      return()
  }

  # add all grouping vars together
  merged_group_vars <-
    c(
      "predictor", # always has to be there
      group_vars,
      col_var
    ) %>%
    unique()

  # get summary for continents
  data_summed <-
    get_r2_summary_varpar(
      data_source = data_source,
      group_vars = merged_group_vars
    )

  # make a plot
  p_0 <-
    data_summed %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = get(paste0(sel_mode, "_mean")),
        x = predictor,
        fill = as.factor(get(col_var)),
        col = as.factor(get(col_var))
      )
    ) +
    ggplot2::facet_wrap(
      as.formula(
        paste(
          "~",
          paste(plot_group_vars, collapse = " + ")
        )
      )
    ) +
    ggplot2::theme_light() +
    ggplot2::coord_polar() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      y = paste("adjR2", sel_mode, sep = " - "),
      fill = col_var,
      col = col_var
    )

  if (
    isTRUE(full_scale)
  ) {
    p_1 <-
      p_0
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      minor_breaks = seq(0, 1, by = 0.2)
    )
  } else {
    p_1 <- p_0
  }

  p_2 <-
    p_1 +
    ggplot2::geom_pointrange(
      mapping = ggplot2::aes(
        ymin = get(paste0(sel_mode, "_lwr")),
        ymax = get(paste0(sel_mode, "_upr"))
      ),
      position = ggplot2::position_dodge(width = 0.5),
      fatten = 0,
      size = 3
    ) +
    ggplot2::geom_point(
      size = 7,
      col = "gray30",
      position = ggplot2::position_dodge(width = 0.5),
    ) +
    ggplot2::geom_point(
      size = 6,
      position = ggplot2::position_dodge(width = 0.5),
    )

  return(p_2)
}
