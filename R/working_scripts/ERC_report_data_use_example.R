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
  dplyr::filter(
    region %in% c(
      "Asia",
      "Europe",
      "North America"
      )
    )
    
# source compositional turnover estimate from targets
data_dcca <-
  targets::tar_read(
    name = data_dcca,
    store = external_storage_targets
    ) %>%
  inner_join(
    data_filtered %>%
      dplyr::select(
        dataset_id,
        lat,
        long,
        region,
        climate_zone = ecozone_koppen_5,
        levels
        ),
    by = "dataset_id"
  )


# Base map with Köppen-Geiger 5 climate zones (in Beck et al. 2018)
# Latitudinal and longitudinal trend in DCCA gradient length
plot_dcca_europe <-
  plot_spatial_dist(
    data_source = data_dcca %>% 
      dplyr::filter(
        region == "Europe"
        ),
    base_map = base_map,
    var_name = "dcca_grad_length",
    lab_name = "DCCA gradient length",
    error_family = "mgcv::Tweedie(p = 1.1, link = 'log')"
    )

ggsave(
  plot_dcca_europe,
  file = paste(
    "Data_summary_outputs/Figure/",
    "Europe_data_filtered_gradient_length_250823.tiff",
    sep = ""
    ),
  dpi = 400,
  compress = "lzw",
  width = 18,
  height = 12,
  unit = "cm"
  )


plot_dcca_north_america <-
  plot_spatial_dist(
    data_source = data_dcca %>% 
      dplyr::filter(
        region == "North America"
      ),
    var_name = "dcca_grad_length",
    lab_name = "DCCA gradient length",
    error_family = "mgcv::Tweedie(p = 1.1, link = 'log')"
  )

ggsave(
  plot_dcca_north_america,
  file = paste(
    "Data_summary_outputs/Figure/",
    "North_America_data_filtered_gradient_length_130923.tiff",
    sep = ""
  ),
  dpi = 400,
  compress = "lzw",
  width = 23,
  height = 10,
  unit = "cm"
)

plot_dcca_asia <-
  plot_spatial_dist(
    data_source = data_dcca %>% 
      dplyr::filter(
        region == "Asia"
      ),
    var_name = "dcca_grad_length",
    lab_name = "DCCA gradient length",
    error_family = "mgcv::Tweedie(p = 1.1, link = 'log')"
  )

ggsave(
  plot_dcca_asia,
  file = paste(
    "Data_summary_outputs/Figure/",
    "Asia_data_filtered_gradient_length_130923.tiff",
    sep = ""
  ),
  dpi = 400,
  compress = "lzw",
  width = 20,
  height = 10,
  unit = "cm"
)


# Temporal trend in compositional turnover
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
                climate_zone,
                region,
                dcca_scores) %>% 
  tidyr::unnest(dcca_scores) %>% 
  dplyr::mutate_at("dataset_id", as.factor)

climate_zone_vec <- 
  c(
    "Cold", 
    "Temperate", 
    "Polar", 
    "Arid", 
    "Tropical"
    ) %>% 
  rlang::set_names()

regions_vec <- 
  c(
    "Asia",
    "Europe",
    "North America"
  ) %>% 
  rlang::set_names()

data_mod <-
  purrr::walk(
    .x = regions_vec,
    .f = ~ {
      
      sel_region <- .x
      
      message(sel_region)
      
      purrr::walk(
        .x = climate_zone_vec, 
        .f = ~ {
          
          sel_ecozone <- .x
          
          sel_data <-
            data_axis1 %>%
            dplyr::filter(region == sel_region) %>% 
            dplyr::filter(climate_zone == sel_ecozone) 
          
          message(sel_ecozone)
          
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
                sel_region,
                "/",
                sel_ecozone,
                "_dcca1",
                ".rds"
              ),
              compress = "gz"
            )
            
            return(data_mod)
          }
        }
      )
    }
  )
  

# Predict the models
data_mod_asia<-
  purrr::map(.x = climate_zone_vec,
             .f = ~ readr::read_rds(
               file = paste0(
                 here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                 "/",
                 "Asia",
                 "/",
                 .x,
                 "_dcca1",
                 ".rds"
               )
              )
             )

data_mod_europe <-
  purrr::map(.x = climate_zone_vec[-5],
             .f = ~ readr::read_rds(
               file = paste0(
                 here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                 "/",
                 "Europe",
                 "/",
                 .x,
                 "_dcca1",
                 ".rds"
                 )
               )
             )
data_mod_na <-
  purrr::map(.x = climate_zone_vec,
             .f = ~ readr::read_rds(
               file = paste0(
                 here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                 "/",
                 "North America",
                 "/",
                 .x,
                 "_dcca1",
                 ".rds"
               )
             )
            )

data_mod_to_predict <- 
  list(
    Asia = data_mod_asia,
    Europe = data_mod_europe,
    "North America" = data_mod_na
    )

data_ecozone_predicted <-
  purrr::map(
    .x = regions_vec,
    .f = ~ {
      sel_region <- .x
      
      message(sel_region)
      
      purrr::map_dfr(
        .x = names(data_mod_to_predict[[sel_region]]) %>% 
          rlang::set_names(),
        .id = "climate_zone",
        .f = ~ {
          sel_ecozone <- .x
          
          message(sel_ecozone)
          
          sel_data <-
            data_axis1 %>%
            dplyr::filter(region == sel_region) %>% 
            dplyr::filter(climate_zone == sel_ecozone)
               
          # New data for predicting
          new_data_general <-
            tibble::tibble(
              age = seq(0, 12000, by = 100)
              )
          data_pred <-
            REcopol::predic_model(
              model_source = data_mod_to_predict[[sel_region]][[sel_ecozone]],
              data_source = new_data_general %>%
                dplyr::mutate(dataset_id = sel_data$dataset_id[1]),
              exclude_var = data_mod_to_predict[[sel_region]][[sel_ecozone]] %>%
                gratia::smooths() %>%
                stringr::str_subset(., "dataset_id")
            ) %>%
            dplyr::rename(DCCA_axis_1 = fit) %>% 
            dplyr::mutate(
              region = sel_region
            )
          
          return(data_pred)
          }
        )
      }
  )
data_to_plot <- 
  data_ecozone_predicted %>%
  bind_rows()

plot_temporal_dcca <-
  data_to_plot %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = age,
      y = DCCA_axis_1
      ),
    group = region
    ) +
  ggplot2::geom_ribbon(
    data = data_to_plot,
    ggplot2::aes(
      ymin = lwr,
      ymax = upr,
      fill = climate_zone,
      group = climate_zone
      ),
    colour = NA,
    alpha = 0.2
    ) + 
  ggplot2::geom_line(
    data = data_to_plot,
    ggplot2::aes(
      group = climate_zone,
      colour = climate_zone
      ),
    linewidth = 1
    ) + 
  ggplot2::geom_smooth(
    linewidth = 1.5,
    colour = "red",
    se = FALSE
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_fill_manual(
    values = palette_ecozone 
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozone 
  ) + 
  facet_wrap(~ region,
             scale = "free_y") + 
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
    strip.text.x = ggplot2::element_text(
      color = "black",
      size = 18),
    axis.text.x = ggplot2::element_text(
      color = "black",
      size = 16,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = ggplot2::element_text(
      color = "black",
      size = 16
    ),
    axis.title = ggplot2::element_text(
      color = "black",
      size = 18
    ),
    legend.position = "bottom",
    legend.key.size = unit(0.6, "cm"),
    legend.title = ggplot2::element_text(
      size = 16
    ),
    legend.text = ggplot2::element_text(
      size = 14
    )
  )

ggsave(
  plot_temporal_dcca,
  file = paste(
    "Data_summary_outputs/Figure/",
    "Data_filtered_turnover_temporal_200923.tiff",
    sep = ""
    ),
  dpi = 400,
  compress = "lzw",
  width = 25,
  height = 10,
  unit = "cm"
  )


# Rate of change (RoC)
data_roc <-
  targets::tar_read(
    name = data_roc,
    store = external_storage_targets
    ) %>%
  inner_join(
    data_filtered %>%
      dplyr::select(
        dataset_id,
        lat,
        long,
        region,
        climate_zone = ecozone_koppen_5
        ), 
    by = "dataset_id"
    ) %>% 
  tidyr::unnest(PAP_roc) %>% 
  dplyr::mutate_at("dataset_id", as_factor)


data_mod_roc <-
  purrr::walk(
    .x = regions_vec[3],
    .f = ~ {
      sel_region <- .x
      
      message(sel_region)
      
      purrr::walk(
        .x = climate_zone_vec,
        .f = ~ {
          sel_ecozone <- .x
          sel_data <-
            data_roc %>%
            dplyr::filter(region == sel_region) %>%
            dplyr::filter(climate_zone == sel_ecozone)
          
          message(sel_ecozone)
          
          if (nrow(sel_data) > 0) {
            
            # Fit GAM model
            data_mod <-
              REcopol::fit_hgam(
                x_var = "Age",
                y_var = "ROC",
                group_var = "dataset_id",
                smooth_basis = "tp",
                data_source = sel_data,
                error_family = "mgcv::Tweedie(p = 1.1)",
                sel_k = 5,
                common_trend = TRUE,
                use_parallel = FALSE
                )
            
            readr::write_rds(
              x = data_mod,
              file = paste0(
                here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                "/",
                sel_region,
                "/",
                sel_ecozone,
                "_roc",
                ".rds"
                ),
              compress = "gz"
              )
            return(data_mod)
            }
          }
        )
      }
    )


# Predict the roc models
data_mod_roc_asia<-
  purrr::map(.x = climate_zone_vec,
             .f = ~ readr::read_rds(
               file = paste0(
                 here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                 "/",
                 "Asia",
                 "/",
                 .x,
                 "_roc",
                 ".rds"
               )
             )
  )

data_mod_roc_europe <-
  purrr::map(.x = climate_zone_vec[-5],
             .f = ~ readr::read_rds(
               file = paste0(
                 here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                 "/",
                 "Europe",
                 "/",
                 .x,
                 "_roc",
                 ".rds"
               )
             )
  )
data_mod_roc_na <-
  purrr::map(.x = climate_zone_vec,
             .f = ~ readr::read_rds(
               file = paste0(
                 here::here("Data_summary_outputs/Data/Models_per_ecozone"),
                 "/",
                 "North America",
                 "/",
                 .x,
                 "_roc",
                 ".rds"
               )
             )
          )

data_mod_roc_to_predict <- 
  list(
    Asia = data_mod_roc_asia,
    Europe = data_mod_roc_europe,
    "North America" = data_mod_roc_na
  )


data_roc_ecozone_predicted <-
  purrr::map(
    .x = regions_vec,
    .f = ~ {
      sel_region <- .x
      
      message(sel_region)
      
      purrr::map_dfr(
        .x = names(data_mod_roc_to_predict[[sel_region]]) %>% 
          rlang::set_names(),
        .id = "climate_zone",
        .f = ~ {
          sel_ecozone <- .x
          
          message(sel_ecozone)
          
          sel_data <-
            data_roc %>%
            dplyr::filter(region == sel_region) %>%
            dplyr::filter(climate_zone == sel_ecozone)
          
          # New data for predicting
          new_data_general <-
            tibble::tibble(
              Age = seq(0, 12000, by = 100)
            )
          data_pred <-
            REcopol::predic_model(
              model_source = data_mod_roc_to_predict[[sel_region]][[sel_ecozone]],
              data_source = new_data_general %>%
                dplyr::mutate(dataset_id = sel_data$dataset_id[1]),
              exclude_var = data_mod_roc_to_predict[[sel_region]][[sel_ecozone]] %>%
                gratia::smooths() %>%
                stringr::str_subset(., "dataset_id")
            ) %>%
            dplyr::rename(RoC = fit) %>% 
            dplyr::mutate(
              region = sel_region
            )
          
          return(data_pred)
        }
      )
    }
  )

data_roc_to_plot <- 
  data_roc_ecozone_predicted %>%
  bind_rows()

plot_temporal_roc <-
  data_roc_to_plot %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = Age,
      y = RoC
    ),
    group = region
  ) +
  ggplot2::geom_ribbon(
    data = data_roc_to_plot,
    ggplot2::aes(
      ymin = lwr,
      ymax = upr,
      fill = climate_zone,
      group = climate_zone
    ),
    colour = NA,
    alpha = 0.2
  ) + 
  ggplot2::geom_line(
    data = data_roc_to_plot,
    ggplot2::aes(
      group = climate_zone,
      colour = climate_zone
    ),
    linewidth = 1
  ) + 
  ggplot2::geom_smooth(
    linewidth = 1.5,
    colour = "red",
    se = FALSE
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_fill_manual(
    values = palette_ecozone 
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozone 
  ) + 
  facet_wrap(~ region,
             scale = "free_y") + 
  ggplot2::labs(
    x = "Time (cal yr BP)",
    y = "Rate of change (RoC)"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 12000, 2000),
    labels = seq(0, 12000, 2000)
  ) +
  labs(fill = "Climate zone",
       colour = "Climate zone") + 
  
  ggplot2::theme(
    strip.text.x = ggplot2::element_text(
      color = "black",
      size = 18),
    axis.text.x = ggplot2::element_text(
      color = "black",
      size = 16,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = ggplot2::element_text(
      color = "black",
      size = 16
    ),
    axis.title = ggplot2::element_text(
      color = "black",
      size = 18
    ),
    legend.position = "bottom",
    legend.key.size = unit(0.6, "cm"),
    legend.title = ggplot2::element_text(
      size = 16
    ),
    legend.text = ggplot2::element_text(
      size = 14
    )
  )

ggsave(
  plot_temporal_roc,
  file = paste(
    "Data_summary_outputs/Figure/",
    "Data_filtered_roc_temporal_200923.tiff",
    sep = ""
  ),
  dpi = 400,
  compress = "lzw",
  width = 25,
  height = 10,
  unit = "cm"
)

