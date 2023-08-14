plot_data_events <- function(data_source_events,
                             select_region = NULL,
                             data_raw = TRUE)  {
  if(data_raw == TRUE) {
    data <- data_source_events %>%
      unnest(data_to_fit) %>%
      inner_join(targets::tar_read(name = "data_meta",
                                   store = paste0(data_storage_path,"_targets_h1")) %>%
                   dplyr::select(dataset_id, 
                                 long, 
                                 lat, 
                                 region, 
                                 ecozone_koppen_5), 
                 by = "dataset_id") %>%
      filter(region == select_region) 
  } else {
    data <- 
      data_source_events %>%
      unnest(data) %>%
      inner_join(targets::tar_read(name = "data_meta",
                                   store = paste0(data_storage_path,"_targets_h1"))  %>%
                   dplyr::select(dataset_id, 
                                 long, 
                                 lat, 
                                 region, 
                                 ecozone_koppen_5), 
                 by = "dataset_id") %>%
      pivot_longer(bi:ei, names_to = "var_name", values_to = "value") %>%
      filter(region == select_region) %>%
      mutate(value = round(value)) 
  }
  
  fig <- data %>%
    ggplot(aes(x = age, y = value, col = var_name)) +
    geom_point() +
    scale_colour_hue(c = 50, l = 50, h = c(30, 300)) +  
    geom_smooth(method = "gam", 
                se = FALSE,
                formula = y ~ s(x, bs = "cs"), 
                method.args = list(family = 
                                     stats::binomial(link = "logit"))) +
    facet_wrap(~ecozone_koppen_5) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(title = select_region, x = "")
  
  fig
  
}
