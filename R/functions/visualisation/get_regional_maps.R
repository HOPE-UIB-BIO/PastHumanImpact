# function to get regional maps
# input is raster data for ecozones

get_map_region <- function(rasterdata = data_geo_koppen,
                           select_region = "Europe") {
  
  
  # order ecozone to get the same colours for the different regions
  order_ecozones <- c("Polar", "Cold", "Temperate", "Arid", "Tropical")
  
  # data for ecozones; order ecozones
  data_geo_koppen <- 
    data_geo_koppen %>%
    mutate(ecozone_koppen_5 = factor(ecozone_koppen_5, 
                                     levels = order_ecozones))
  
  #regional limits
  region_limits <- tibble(
    region = c("North America", "Europe", "Asia", "Latin America", "Oceania"),
    xmin = c(-170, -10, 30, -103, 110),
    xmax = c(-50, 50, 180, -23, 154),
    ymin = c(10, 30, 0, -56, -50),
    ymax = c(89, 80, 80, 34, -3)
  )
  
  boundary <- region_limits %>%
    filter(region %in% select_region)
  
  map_region <- 
    rasterdata %>%
    ggplot() +
    geom_raster(aes(x = x,
                    y = y,
                    fill = ecozone_koppen_5)) +
    scale_fill_manual(values = palette_eco, drop = FALSE) +
    coord_sf(expand = TRUE,
             ylim = c(boundary$ymin[1], boundary$ymax[1]),
             xlim = c(boundary$xmin[1], boundary$xmax[1])) +
    theme_void() +
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.spacing=unit(c(0,0,0,0), "null"),
      plot.margin=grid::unit(c(0,1,0,0), "cm"),
    )
  map_region
}
