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
    "R/00_Config_file.R"
  )
)

# Import tables for plotting
source(
  here::here(
    "R/working_scripts/Results_script.R"
  )
)

plot_predictor <- function(
    data_source_raw,
    data_source_pred, sel_predictor,
    sel_y_limits = c(-20, 20)) {
  data_mean_sd <-
    data_source_pred %>%
    tidyr::unnest(data_pred) %>%
    dplyr::filter(predictor == sel_predictor) %>%
    dplyr::group_by(
      region,
      sel_classification
    ) %>%
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        "fit",
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE)
        )
      )
    )

  data_raw_mean_sd <-
    data_source_raw %>%
    dplyr::left_join(
      data_mean_sd,
      by = c("region", "sel_classification")
    ) %>%
    dplyr::mutate(
      fit_rescale = (get(sel_predictor) - fit_mean) / fit_sd
    )


  data_source_pred %>%
    tidyr::unnest(data_pred) %>%
    dplyr::filter(predictor == sel_predictor) %>%
    dplyr::left_join(
      data_mean_sd,
      by = c("region", "sel_classification")
    ) %>%
    dplyr::mutate(
      fit_rescale = (fit - fit_mean) / fit_sd,
      upr_rescale = (upr - fit_mean) / fit_sd,
      lwr_rescale = (lwr - fit_mean) / fit_sd
    ) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = age,
        y = fit_rescale,
        col = sel_classification,
        fill = sel_classification
      )
    ) +
    ggplot2::facet_grid(
      region ~ sel_classification,
      scales = "free_y"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 8.5e3),
      breaks = c(seq(0, 8.5e3, by = 2000)),
      labels = c(seq(0, 8.5, by = 2))
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
      y = "Centrelasied and standardised values (sd units)"
    ) +
    ggplot2::geom_line(
      data = data_raw_mean_sd,
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
}

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_merge_unnest <-
  targets::tar_read(
    name = "data_merge_unnest",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  ) %>%
  dplyr::rename(sel_classification = group) %>%
  dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "sel_classification"
  )

mod_predicted_with_names <-
  targets::tar_read(
    name = "mod_predicted_with_names",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  ) %>%
  dplyr::rename(sel_classification = group) %>%
  dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "sel_classification"
  )

#----------------------------------------------------------#
# 2. Plot individual figures -----
#----------------------------------------------------------#

list_fig_predictors <-
  c(
    "temp_annual",
    "temp_cold",
    "prec_summer",
    "prec_win",
    "spd"
  ) %>%
  rlang::set_names() %>%
  purrr::map(
    .progress = TRUE,
    .f = ~ plot_predictor(
      data_source_raw = data_merge_unnest,
      data_source_pred = mod_predicted_with_names,
      sel_predictor = .x
    )
  )

list_fig_predictors[[1]]

list_fig_predictors %>%
  purrr::iwalk(
    .progress = TRUE,
    .f = ~ ggplot2::ggsave(
      paste0(
        here::here("Outputs/Supp"),
        "/",
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
