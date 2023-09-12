# m2 correlation full dataset



data_meta <- 
  targets::tar_read(data_meta,
                    store = paste0(
                    data_storage_path,
                    "_targets_h1"
                               ))

data_meta <- data_meta %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) 


palette_ecozones <- 
  c( Polar = "#009292", 
     Cold_Without_dry_season_Very_Cold_Summer = "#004949", 
     Cold_Without_dry_season_Cold_Summer = "#006ddb", 
     Cold_Dry_Winter = "#6db6ff",
     Cold_Dry_Summer = "#b6dbff",
     Cold_Without_dry_season_Warm_Summer = "#117733",
     Cold_Without_dry_season_Hot_Summer = "#999933", 
     Temperate_Without_dry_season = "#DDCC77",
     Temperate_Dry_Winter = "#b66dff",
     Temperate_Dry_Summer = "#ffff6d",
     Arid = "#924900",  
     Tropical =  "#920000"
  )



get_data_properties <- function(data_source_diversity,
                                data_source_roc,
                                data_source_density,
                                used_rescale = TRUE) {
  
  
  
  if(
    isTRUE(used_rescale)
  ) {
    data_source_density <- data_source_density %>%
      dplyr::select(dataset_id, pap_density_rescale) %>%
      dplyr::rename(density = pap_density_rescale)
  } else {
    data_source_density <- data_source_density %>%
      dplyr::select(dataset_id, pap_density) %>%
      dplyr::rename(density = pap_density)
    
  }
  
  dplyr::inner_join(
    data_source_diversity %>%
      dplyr::rename(
        diversity = data
      ),
    data_source_roc %>%
      dplyr::rename(
        roc = data
      ),
    by = "dataset_id"
  ) %>%
    dplyr::inner_join(
      data_source_density,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      data_merge = purrr::pmap(
        .l = list(
          diversity, # ..1
          roc, # ..2
          density #..3
        ),
        .f = ~ dplyr::inner_join(
          ..1,
          ..2,
          by = "age"
        ) %>%
          dplyr::inner_join(
            ..3,
            by = "age"
          )  %>%
          drop_na()
      ) 
    ) %>%
    dplyr::select(
      dataset_id, data_merge
    ) %>%
    
    return()
}

# get full data for properties (but interpolated)
data_properties <- 
  get_data_properties(data_source_diversity =  targets::tar_read(data_div_dcca_interpolated,
                                                               store = paste0(
                                                                 data_storage_path,
                                                                 "_targets_h1"
                                                               )),
                    data_source_roc = targets::tar_read(data_roc_interpolated,
                                                        store = paste0(
                                                          data_storage_path,
                                                          "_targets_h1"
                                                        )),
                    data_source_density = targets::tar_read(data_density_estimate,
                                                            store = paste0(
                                                              data_storage_path,
                                                              "_targets_h1"
                                                            )),
                    used_rescale = TRUE
                    
                    )


# GET PROCRUSTES M2
vec_responses <-
  c(
    "dataset_id",
    "age",
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "dcca_axis_1", "roc",
    "density_turnover",
    "density_diversity"
  )


data_m2 <- get_data_m2(
  data_source = data_properties,
  data_meta = data_meta,
  select_vars = vec_responses
)

data_m2

m2_change_region <- 
  data_m2 %>%
  filter(!region == "Africa") %>%
  dplyr::select(m2_time_df, 
                region, 
                sel_classification) %>%
  mutate(region = factor(region, levels = c("North America",
                                            "Latin America",
                                            "Europe", 
                                            "Asia", 
                                            "Oceania"))) %>%
  unnest(cols = c(m2_time_df)) %>%
  ggplot(aes(x = as.numeric(time), 
             y = delta_m2, 
             col = sel_classification, 
             fill = sel_classification)) +
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  scale_x_continuous(breaks = c(seq(0, 12000, by = 2000))) +
  
  theme_classic() +
  scale_color_manual(values = palette_ecozones, drop = FALSE) +
  scale_fill_manual(values = palette_ecozones, drop = FALSE) +
  theme(
    strip.background = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 10)) +
  facet_wrap(~region, nrow = 5, scales = "free") +
  labs(x = "Time years BP", y = "change in m2")



ggsave(
  paste0("m2_change_time_regions.png"),
  m2_change_region,
  width = 8, height = 12, units = "cm",
  scaling = 0.5,
  bg = "white"
)

