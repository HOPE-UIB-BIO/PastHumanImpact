# get world map with ecozones
get_world_map_ecozones <- function(rasterdata = data_geo_koppen){
  
  map_world <- 
    rasterdata %>%
    ggplot() +
    geom_raster(aes(x = x,
                    y = y,
                    fill = ecozone_koppen_5)) +
    scale_fill_manual(name = "Ecozones", 
                      values = palette_eco) +
    coord_sf(expand = TRUE) +
    theme_void() +
    theme(
      legend.position = "bottom",
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.spacing=unit(c(0,0,0,0), "null"),
      plot.margin=grid::unit(c(0,1,0,0), "cm"),
    )
  
  map_world
  
}