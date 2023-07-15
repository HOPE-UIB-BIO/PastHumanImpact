# function to get quick regional summary figures

get_regional_combined_fig <- function(data_spatial = summary_spatial_median,
                                      data_temporal = summary_temporal_median,
                                      select_region = "Europe") {
  
  order_predictors_spatial <- c("human", "climate", "time")
  order_ecozones <- c("Polar", "Cold", "Temperate", "Arid", "Tropical")
  x_label <- c("Human", "Climate", "Time")
  
  # filter spatial input data
  input_spatial <- data_spatial %>% 
    mutate(predictor = factor(predictor, 
                              levels = order_predictors_spatial)) %>%
    mutate(ecozone_koppen_5 = factor(ecozone_koppen_5, 
                                     levels = order_ecozones)) %>%
    dplyr::filter(region %in% select_region) %>%
    filter(n_records > 5)
  
  circular_bar_fig <- get_circular_barchart(input_spatial)
  
  # temporal bar 
  # filter temporal input data
  input_temporal <- 
    data_temporal %>%
    filter(region %in% select_region) %>%
    mutate(predictor =  factor(predictor, 
                               levels = c("human", "climate"))) 
  
  bars_temporal_fig <- get_temporal_barcharts(input_temporal)
  
  # Regional maps
  map_region <- get_map_region(select_region = select_region)
  
  # Combine figures 
  final <- 
    ggpubr::ggarrange(
      ggpubr::ggarrange(
        circular_bar_fig,
        ggpubr::ggarrange(map_region, 
                          NULL, 
                          ncol = 1,
                          heights = c(2,1)
        ), 
        ncol = 2,
        widths = c(2,1)),
      bars_temporal_fig,
      nrow = 2,
      heights = c(2,1)
    )
  
  final
}