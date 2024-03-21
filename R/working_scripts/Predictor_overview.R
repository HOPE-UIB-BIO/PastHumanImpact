#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#           Supplementary: H2 predictor overview
#
#                   V. Felde, O. Mottl
#                         2023
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)


plot_predictor <- function(
    data_source_raw,
    data_source_pred,
    sel_predictor,
    plot_raw = FALSE,
    sel_y_limits = c(-20, 20)) {
  if (
    isTRUE(plot_raw)
  ) {
    data_to_plot_raw <-
      data_source_raw %>%
      dplyr::filter(variable == sel_predictor) %>%
      dplyr::mutate(
        fit_rescale = value
      )

    data_to_plot_pred <-
      data_source_pred %>%
      dplyr::filter(variable == sel_predictor) %>%
      dplyr::mutate(
        fit_rescale = value,
        upr_rescale = conf_high,
        lwr_rescale = conf_low
      )
  } else {
    data_pred_mean_sd <-
      data_source_pred %>%
      tidyr::unnest(data_pred) %>%
      dplyr::filter(variable == sel_predictor) %>%
      dplyr::group_by(
        region,
        climatezone
      ) %>%
      dplyr::summarise(
        .groups = "drop",
        dplyr::across(
          "value",
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE)
          )
        )
      )

    data_to_plot_raw <-
      data_source_raw %>%
      dplyr::left_join(
        data_pred_mean_sd,
        by = c("region", "climatezone")
      ) %>%
      dplyr::mutate(
        fit_rescale = (get(sel_predictor) - value_mean) / value_sd
      )


    data_to_plot_pred <-
      data_source_pred %>%
      dplyr::filter(variable == sel_predictor) %>%
      dplyr::left_join(
        data_pred_mean_sd,
        by = c("region", "climatezone")
      ) %>%
      dplyr::mutate(
        fit_rescale = (value - value_mean) / value_sd,
        upr_rescale = (conf_high - value_mean) / value_sd,
        lwr_rescale = (conf_low - value_mean) / value_sd
      )
  }

  fig_res <-
    data_to_plot_pred %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = age,
        y = fit_rescale,
        col = climatezone,
        fill = climatezone
      )
    ) +
    ggplot2::facet_grid(
      region ~ climatezone,
      scales = "free_y"
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      limits = c(8.5e3, 0),
      breaks = c(seq(8.5e3, 0, by = -2000)),
      labels = c(seq(8.5, 0, by = -2))
    ) +
    ggplot2::coord_cartesian(
      ylim = sel_y_limits
    ) +
    ggplot2::scale_color_manual(
      values = palette_ecozones
    ) +
    ggplot2::scale_fill_manual(
      values = palette_ecozones
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Age (ka cal BP years)",
      y = "Centrelasied and standardised values (sd units)",
      caption = paste(
        "Thin lines represent the raw data.",
        "Thick lines represent the predicted model."
      )
    ) +
    ggplot2::geom_line(
      data = data_to_plot_raw,
      mapping = ggplot2::aes(
        group = dataset_id
      ),
      alpha = 0.3,
      linewidth = 0.1
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = lwr_rescale,
        ymax = upr_rescale
      ),
      col = "transparent",
      alpha = 0.3
    ) +
    ggplot2::geom_line(
      linewidth = 1
    )

  if (
    isTRUE(plot_raw)
  ) {
    fig_res <-
      fig_res +
      ggplot2::labs(
        y = paste("Raw values of", sel_predictor)
      )
  }

  return(fig_res)
}

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

data_predictors_raw_data <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_to_fit",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  ) %>%
  dplyr::mutate(
    climatezone = as.factor(climatezone)
  ) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "climatezone"
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    )
  ) %>%
  dplyr::filter(
    region != "Africa"
  ) %>%
  tidyr::unnest(data_to_fit) %>%
  dplyr::select(
    region, climatezone, dataset_id, age, variable, value
  )

mod_config_file <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

data_general_tredns_raw <-
  get_all_predicted_general_trends(
    data_source = mod_config_file,
    sel_type = "predictors"
  )

data_general_tredns <-
  data_general_tredns_raw %>%
  dplyr::mutate(
    climatezone = as.factor(climatezone)
  ) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "climatezone"
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    )
  ) %>%
  dplyr::filter(
    region != "Africa"
  )



#----------------------------------------------------------#
# 2. Plot individual figures -----
#----------------------------------------------------------#
list_fig_predictors <-
  purrr::pmap(
    .progress = TRUE,
    .l = list(
      variable = c(
        "temp_annual",
        "temp_cold",
        "prec_summer",
        "prec_win",
        "spd"
      ) %>%
        rlang::set_names(),
      plot_raw = rep(TRUE, 5),
      y_limits = list(
        c(-10, 20),
        c(-30, 25),
        c(0, 1000),
        c(0, 2000),
        c(0, 2.5)
      )
    ),
    .f = ~ plot_predictor(
      data_source_raw = data_predictors_raw_data,
      data_source_pred = data_general_tredns,
      sel_predictor = ..1,
      plot_raw = ..2,
      sel_y_limits = ..3
    )
  )

list_fig_predictors %>%
  purrr::iwalk(
    .progress = TRUE,
    .f = ~ ggplot2::ggsave(
      paste0(
        here::here("Outputs/Supp"),
        "/Supplementary_figure_",
        .y,
        ".png"
      ),
      plot = .x,
      width = image_width_vec["3col"], # [config criteria]
      height = 200,
      units = image_units, # [config criteria]
      bg = "white"
    )
  )
