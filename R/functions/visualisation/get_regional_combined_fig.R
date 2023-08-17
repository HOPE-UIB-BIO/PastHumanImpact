# function to get quick regional summary figures

get_regional_combined_fig <- function(data_spatial_h1 = summary_spatial_median,
                                      data_temporal_h1 = summary_temporal_median,
                                      data_hvar_h2 = data_h2_vis,
                                      select_region = "Europe",
                                      order_predictors_spatial = c("human", 
                                                                   "climate", 
                                                                   "time"),
                                      palette_ecozones = palette_ecozones,
                                      x_label = c("Human", "Climate", "Time")
                                      ) {
  
  
  
 
  
  # spatial input data for h1
  input_spatial <- data_spatial_h1 %>% 
    mutate(predictor = factor(predictor, 
                              levels = order_predictors_spatial)) %>%
    dplyr::filter(region %in% select_region) %>%
    filter(n_records > 5)
  
  circular_bar_h1 <- get_circular_barchart(input_spatial,
                                           y_var = "percentage_median",
                                           title = "h1")
  
  # input data for h2
  input_h2 <- data_hvar_h2 %>%
    filter(region %in% select_region)
  
  circular_bar_h2 <- get_circular_barchart(input_h2,
                                           y_var = "percentage",
                                           fill_var = "group",
                                           title = "h2")
  
  # temporal bar 
  # filter temporal input data
  input_temporal <- 
    data_temporal_h1 %>%
    filter(region %in% select_region) %>%
    mutate(predictor =  factor(predictor, 
                               levels = c("human", 
                                          "climate"))) 
  
  bars_temporal_h1 <- get_temporal_barcharts(input_temporal)
  
  # Regional maps
  map_region <- get_map_region(select_region = select_region)
  
  # Combine figures 
  final <- 
    ggpubr::ggarrange(
      ggpubr::ggarrange(
        circular_bar_h1,
        circular_bar_h2,
        ggpubr::ggarrange(map_region, 
                          NULL, 
                          ncol = 1,
                          heights = c(2,1)
        ), 
        ncol = 3,
        widths = c(1,1,0.5)),
     ggpubr::ggarrange(bars_temporal_h1,
      NULL,
      ncol = 2, 
      widths = c(2,0.5)),
      nrow = 2,
      heights = c(2,1)
    )
  
  final
}
