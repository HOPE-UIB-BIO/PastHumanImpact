#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    Extended data
#                  PAP temporal trends
#
#                   V. Felde, O. Mottl
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_pap_model_raw <-
  RUtilpol::get_latest_file(
    file_name = "pap_temporal_model_data",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor()

data_pap_predictions <-
  RUtilpol::get_latest_file(
    file_name = "pap_temporal_model_predictions",
    dir = paste0(
      data_storage_path,
      "Temporal_models/General_trends"
    )
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor()


#----------------------------------------------------------#
# 2. Prepare labels -----
#----------------------------------------------------------#

data_pap_model_raw <-
  data_pap_model_raw %>%
  dplyr::mutate(
    pap_label = dplyr::case_when(
      variable == "n0" ~ "Taxonomic richness",
      variable == "n1" ~ "Shannon diversity",
      variable == "n2" ~ "Simpson diversity",
      variable == "n1_minus_n2" ~ "Shannon - Simpson diversity",
      variable == "n2_divided_by_n1" ~ "Simpson / Shannon diversity",
      variable == "n1_divided_by_n0" ~ "Shannon / richness",
      variable == "roc" ~ "Rate of change",
      variable == "dcca_axis_1" ~ "DCCA axis 1",
      variable == "density_diversity" ~ "Density diversity",
      variable == "density_turnover" ~ "Density turnover",
      .default = variable
    )
  )

data_pap_predictions <-
  data_pap_predictions %>%
  dplyr::mutate(
    pap_label = dplyr::case_when(
      variable == "n0" ~ "Taxonomic richness",
      variable == "n1" ~ "Shannon diversity",
      variable == "n2" ~ "Simpson diversity",
      variable == "n1_minus_n2" ~ "Shannon - Simpson diversity",
      variable == "n2_divided_by_n1" ~ "Simpson / Shannon diversity",
      variable == "n1_divided_by_n0" ~ "Shannon / richness",
      variable == "roc" ~ "Rate of change",
      variable == "dcca_axis_1" ~ "DCCA axis 1",
      variable == "density_diversity" ~ "Density diversity",
      variable == "density_turnover" ~ "Density turnover",
      .default = variable
    )
  )


#----------------------------------------------------------#
# 3. Build and save figures -----
#----------------------------------------------------------#

purrr::walk(
  .x = unique(data_pap_predictions[["variable"]]),
  .f = ~ {
    sel_variable <- .x

    data_raw_selected <-
      data_pap_model_raw %>%
      dplyr::filter(variable == sel_variable)

    data_prediction_selected <-
      data_pap_predictions %>%
      dplyr::filter(variable == sel_variable)

    sel_label <-
      data_prediction_selected %>%
      dplyr::distinct(pap_label) %>%
      dplyr::pull(pap_label) %>%
      dplyr::first()

    plot_pap_temporal <-
      data_prediction_selected %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = age,
          y = estimate,
          col = climatezone,
          fill = climatezone
        )
      ) +
      ggplot2::facet_grid(
        region ~ climatezone_label,
        scales = "free_y",
        labeller = ggplot2::labeller(
          region = ggplot2::as_labeller(
            region_labeller,
            default = ggplot2::label_wrap_gen(15)
          ),
          climatezone_label = ggplot2::label_wrap_gen(7)
        )
      ) +
      ggplot2::scale_x_continuous(
        trans = "reverse",
        limits = c(8.5e3, 0),
        breaks = seq(8e3, 0, by = -2e3),
        labels = seq(8, 0, by = -2)
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
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(
          fill = "transparent",
          color = "transparent"
        ),
        strip.text = ggplot2::element_text(
          size = text_size,
          color = common_gray
        ),
        axis.title = ggplot2::element_text(
          size = text_size,
          color = common_gray
        ),
        axis.text = ggplot2::element_text(
          size = text_size,
          color = common_gray
        ),
        text = ggplot2::element_text(
          size = text_size,
          color = common_gray
        ),
        line = ggplot2::element_line(
          linewidth = line_size,
          color = common_gray
        )
      ) +
      ggplot2::labs(
        x = "Age (cal ka BP)",
        y = sel_label
      ) +
      ggplot2::geom_line(
        data = data_raw_selected,
        mapping = ggplot2::aes(
          y = value,
          group = dataset_id
        ),
        alpha = 0.2,
        linewidth = line_size
      ) +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(
          ymin = conf_low,
          ymax = conf_high
        ),
        col = "transparent",
        alpha = 0.25
      ) +
      ggplot2::geom_line(
        linewidth = 0.4
      )

    purrr::walk(
      .x = c("png", "pdf"),
      .f = ~ ggplot2::ggsave(
        paste(
          here::here(
            "Outputs/Figures/Extended_data_figures",
            paste0("PAP_through_time_", sel_variable)
          ),
          .x,
          sep = "."
        ),
        plot = plot_pap_temporal,
        width = image_width_vec["3col"],
        height = 200,
        units = image_units,
        bg = "white"
      )
    )
  }
)
