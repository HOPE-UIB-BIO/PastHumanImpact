#----------------------------#
# Example data use ----
#----------------------------#
library(here)
source("R/00_Config_file.R")

# load data for Europe
data_filtered <- 
  targets::tar_read(
    name = data_assembly_filtered,
    store = external_storage_targets
    ) %>% 
  dplyr::filter(region == "Europe")


# estimate total compositional turnover
data_dcca <-
  data_filtered %>%
  dplyr::mutate(
    percentages =
      purrr::map(
        .x = counts_harmonised,
        .f = function(.x) {
          counts <-
            .x %>%
            column_to_rownames("sample_id")
          percentages <-
            ((counts / rowSums(counts)) * 100) %>%
            round(., digits = 3) %>%
            mutate_all(~ replace(., is.nan(.),
                                 0)) %>%
            rownames_to_column("sample_id") %>%
            dplyr::select(sample_id,
                          everything())
          return(percentages)
        }
      ),
    dcca = purrr::map2(
      .x = percentages,
      .y = levels,
      .f = ~ REcopol::fit_ordination(
        data_source_community = .x,
        data_source_predictors = .y,
        sel_method = "constrained",
        var_name_pred = "age",
        sel_complexity = "poly_2",
        transform_to_percentage = FALSE,
        tranformation = "none"
        )
      ),
    dcca_grad_length = purrr::map_dbl(
      .x = dcca,
      .f = ~ .x %>%
        purrr::pluck("axis_1_grad_length")
      ),
    dcca_scores = purrr::map(
      .x = dcca,
      .f = ~ .x %>% 
        purrr::pluck("case_r")
      )
    ) %>%
  dplyr::select(dataset_id,
                long,
                lat,
                levels,
                ecozone_koppen_5,
                dcca_grad_length,
                dcca_scores
                )

# Base map with Köppen-Geiger 5 climate zones (in Beck et al. 2018)
base_map <-
  data_dcca %>%
  ggplot2::ggplot(ggplot2::aes(x = long,
                               y = lat)) +
  ggplot2::coord_fixed(ylim = c(32.00, 75.00),
                       xlim = c(2, 35.00)) +
  
  ggplot2::labs(x = "Longitude",
                y = "Latitude",
                colour = "Climate zones") +
  ggplot2::theme_classic() +
  ggplot2::borders(colour = "black",
                   size = 0.2) +
  theme(legend.position = "none")


plot_dcca <-
  plot_spatial_dist(
    data_source = data_dcca,
    base_map = base_map,
    var_name = "dcca_grad_length",
    lab_name = "DCCA gradient length",
    error_family = "mgcv::Tweedie(p = 1.1, link = 'log')"
    )

#ggsave(
#  plot_dcca,
#  file = paste(
#    "Data_summary_outputs/Figure/",
#    "Europe_data_filtered_gradient_length_250823.tiff",
#    sep = ""
#    ),
#  dpi = 400,
#  compress = "lzw",
#  width = 15,
#  height = 15,
#  unit = "cm"
#  )


data_axis1 <-
  data_dcca %>%
  dplyr::mutate(
    dcca_scores =
      purrr::map2(
        .x = dcca_scores,
        .y = levels,
        .f = ~ .x %>%
          dplyr::select(sample_id,
                        axis_1) %>%
          inner_join(.y %>%
                       dplyr::select(sample_id,
                                     age),
                     by = "sample_id")
        )
    ) %>% 
  dplyr::select(dataset_id, 
                long,
                lat,
                climate_zone = ecozone_koppen_5,
                dcca_scores) %>% 
  tidyr::unnest(dcca_scores) %>% 
  dplyr::mutate_at("dataset_id", as.factor)

climate_zone_vec <- 
  c("Cold", "Temperate", "Polar", "Arid") %>% 
  rlang::set_names()


data_mod <-
  purrr::walk(
    .x = climate_zone_vec, 
    .f = ~ {
      sel_ecozone <- .x
      
      sel_data <-
        data_axis1 %>%
        dplyr::filter(climate_zone == sel_ecozone) 
      
      message(sel_ecozone)
      
      sel_data$dataset_id %>%
        unique() %>%
        length() %>%
        message()
      
      if (
        nrow(sel_data) > 0
      ) {
        # Fit GAM model
        data_mod <-
          REcopol::fit_hgam(
            x_var = "age",
            y_var = "axis_1",
            group_var = "dataset_id",
            smooth_basis = "tp",
            data_source = sel_data,
            error_family = "mgcv::Tweedie(p = 1.1)",
            sel_k = 10, 
            common_trend = TRUE,
            use_parallel = FALSE
          )
        readr::write_rds(
          x = data_mod,
          file = paste0(
            here::here(
              "Data_summary_outputs/Data/Models_per_ecozone"
            ),
            "/",
            sel_ecozone,
            ".rds"
          ),
          compress = "gz"
        )
        
        return(data_mod)
      }
    }
  )

# Predict the models
data_mod <-
  purrr::map(
    .x = climate_zone_vec,
    .f = ~ readr::read_rds(
      file = paste0(
        here::here("Data_summary_outputs/Data/Models_per_ecozone"),
               "/",
               .x,
               ".rds"
             )
      )
    )

data_ecozone_predicted <-
  purrr::map_dfr(
    .x = climate_zone_vec,
    .id = "climate_zone",
    .f = ~ {
      sel_ecozone <- .x
      
      message(sel_ecozone)
      
      sel_data <-
        data_axis1 %>%
        dplyr::filter(climate_zone == sel_ecozone) 
      
      # New data for predicting
      new_data_general <-
        tibble::tibble(age = seq(0, 12000, by = 100))
      data_pred <-
        REcopol::predic_model(
          model_source = data_mod[[sel_ecozone]],
          data_source = new_data_general %>%
            dplyr::mutate(dataset_id = sel_data$dataset_id[1]),
          exclude_var = data_mod[[sel_ecozone]] %>%
            gratia::smooths() %>%
            stringr::str_subset(., "dataset_id")
          ) %>%
        dplyr::rename(DCCA_axis_1 = fit)
      return(data_pred)
      }
    )


plot_temporal <-
  data_ecozone_predicted %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = age,
      y = DCCA_axis_1,
      group = climate_zone
      )
    ) +
  ggplot2::geom_ribbon(
    data = data_ecozone_predicted,
    ggplot2::aes(
      ymin = lwr,
      ymax = upr,
      fill = climate_zone,
      ),
    colour = NA,
    alpha = 0.2
    ) + 
  ggplot2::geom_line(
    data = data_ecozone_predicted,
    ggplot2::aes(
      colour = climate_zone
      ),
    size = 1
    ) + 
  ggplot2::theme_classic() +
  ggplot2::scale_fill_manual(
    values = palette_ecozone 
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozone 
  ) + 
  ggplot2::labs(
    x = "Time (cal yr BP)",
    y = "DCCA axis-1"
    ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 12000, 2000),
    labels = seq(0, 12000, 2000)
    ) +
  
  labs(fill = "Climate zone",
       colour = "Climate zone") + 
  
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      color = "black",
      size = 12,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = ggplot2::element_text(
      color = "black",
      size = 12
    ),
    axis.title = ggplot2::element_text(
      color = "black",
      size = 14
    ),
    legend.position = "bottom"
  )

ggsave(
  plot_temporal,
  file = paste(
    "Data_summary_outputs/Figure/",
    "Europe_data_filtered_turnover_temporal_250823.tiff",
    sep = ""
    ),
  dpi = 400,
  compress = "lzw",
  width = 15,
  height = 15,
  unit = "cm"
  )
