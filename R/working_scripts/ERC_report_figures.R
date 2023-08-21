#----------------------------#
# Figures of data distribution and data properties ----
#----------------------------#

library(here)
source("R/00_Config_file.R")


# load data 
data_filtered <- 
  targets::tar_read(
    name = data_assembly_filtered,
    store = external_storage_targets
    ) #1377

options(max.print = 9999)

for_analysis <-
  data_filtered %>% 
  dplyr::select(
    dataset_id,
    long,
    lat,
    region,
    harmonisation_region,
    depositionalenvironment,
    n_sample_counts,
    levels,
    age_uncertainty,
    chron_control_format,
    n_chron_control,
    pollen_percentage,
    source_of_data,
    ecozone_koppen_5,
    ecozone_koppen_15,
    raw_counts,
    counts_harmonised
  ) %>% 
  
  # Explicit source of data
  dplyr::mutate(source_of_data_specific = ifelse(
    stringr::str_detect(dataset_id, "cao|PANGAEA|pg_") == TRUE,
    "Pangaea",
    source_of_data
  ),
  source_of_data_specific = ifelse(
    stringr::str_detect(dataset_id,
                        "COLE_etal|SAND|Sandynallah_SBBS|Trench|Lake_Moon") == TRUE,
    "Publication",
    source_of_data_specific
  ),
  source_of_data_specific = ifelse(
    stringr::str_detect(dataset_id,
                        "PVT_|__Latin_America|ippd_|Western_Ghats") == TRUE,
    "Data_contributors",
    source_of_data_specific
  ),
  source_of_data_specific = ifelse(
    source_of_data_specific == "Bhatta-collection" & region == "Asia",
    "Neotoma",
    source_of_data_specific
  )
  )

(for_analysis %>% dplyr::filter(source_of_data_specific == "Neotoma"))$dataset_id #1127
(for_analysis %>% dplyr::filter(source_of_data_specific == "Pangaea"))$dataset_id #63
(for_analysis %>% dplyr::filter(source_of_data_specific == "Data_contributors"))$dataset_id #184
(for_analysis %>% dplyr::filter(source_of_data_specific == "Publication"))$dataset_id #3

(for_analysis %>% dplyr::filter(region == "Asia"))$dataset_id #178
(for_analysis %>% dplyr::filter(region == "Europe"))$dataset_id #397
(for_analysis %>% dplyr::filter(region == "North America"))$dataset_id #558
(for_analysis %>% dplyr::filter(region == "Latin America"))$dataset_id #175
(for_analysis %>% dplyr::filter(region == "Africa"))$dataset_id #20
(for_analysis %>% dplyr::filter(region == "Oceania"))$dataset_id #49

#NOTE: "072__Latin_America" is in Africa

# Distribution of data ----
for_analysis$region <-
  factor(
    for_analysis$region,
    levels = c(
      'Asia',
      'Europe',
      'North America',
      'Latin America',
      'Africa',
      'Oceania'
    )
  )

p1 <-
  for_analysis %>%
  ggplot(
    aes(
      x = long, 
      y = lat, 
      group = ecozone_koppen_5
      )
    ) +
  coord_fixed(
    ylim = c(min(for_analysis$lat), 
             max(for_analysis$lat)),
    xlim = c(min(for_analysis$long), 
             max(for_analysis$long))
    ) +
  labs(
    x = expression(
      paste(
        'Longitude ', (degree ~ E)
        )
      ),
    y = expression(
      paste(
        'Latitude ', (degree ~ N)
        )
      ),
    colour = "Climate zone") +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 50)
    ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 15)
    ) + 
  theme_classic() +
  borders(
    colour = "black",
    linewidth = 0.15
    ) +
  ggtitle("Number of records = 1377, 1357 excluding Africa") +
  geom_point(
    aes(
      colour = ecozone_koppen_5
      ),
    size = 1.5,
    alpha = 0.55
    ) +
  scale_colour_manual(values = palette_ecozone) + 
  
  guides(
    colour = guide_legend(nrow = 1)
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text = element_text(colour = "black",
                             size = 12),
    axis.title = element_text(colour = "black", 
                              size = 14),
    plot.margin = margin(0, 0, 0.1, 0, "cm")
    ) 

p2 <-
  for_analysis %>%
  dplyr::mutate(
    data_type = ifelse(
      pollen_percentage == "FALSE",
      "Counts",
      "Percentages"),
    distribution = "Global"
    ) %>%
  ggplot(
    aes(
      x = fct_infreq(source_of_data_specific),
      fill = data_type)
    ) +
  geom_bar(
    aes(y = after_stat(count)
        )
    ) +
  scale_fill_manual(values = palette_matching[c(1,5)]) +
  labs(fill = "Data type") + 
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    vjust = 0.55,
    size = 3
    ) +
  theme_classic() +
  facet_wrap(~ distribution) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(color = "black",
                               size = 12,),
    axis.text.x = element_text(
      color = "black",
      size = 12,
      angle = 45,
      hjust = 1
      ),
    strip.text = element_text(size = 12, 
                              color = "black"),
    legend.position = c(0.7, 0.8),
    legend.background = element_rect(colour = "transparent",
                                     fill = "transparent"),
    legend.key.size = unit(0.50, "cm")
    )

p3 <-
  p2 +
  facet_wrap(~ region,
              scales = "free_y",
              ncol = 3
             ) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, 
                                  color = "black"
                                  )
        )

p4 <-
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(
      p2,
      p3,
      ncol = 2,
      widths = c(0.25, 0.75),
      left = grid::textGrob(
        "Number of pollen records",
        rot = 90,
        hjust = 0,
        gp = grid::gpar(fontsize = 16)
        ),
      bottom = grid::textGrob(
        "Data source",
        gp = grid::gpar(fontsize = 16)
        )
      )
    )

final_plot <-
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(
      p1,
      p4,
      nrow = 2
      )
    )

#ggsave(final_plot,
#       file = paste("Data_summary_outputs/Figure/",
#                    "Datasets_complete_filtered_210823.tiff",
#                    sep = ""),
#       dpi = 400,
#       compress = "lzw",
#       width = 20,
#       height = 20,
#       unit = "cm")


#EXCLUDE AFRICA
data_to_plot <- 
  for_analysis %>% 
  dplyr::filter(!region == "Africa") #1357

for_analysis$region <-
  factor(
    for_analysis$region,
    levels = c(
      'Asia',
      'Europe',
      'North America',
      'Latin America',
      'Oceania'
      )
    )

p1 <-
  data_to_plot %>%
  ggplot(
    aes(
      x = long, 
      y = lat, 
      group = ecozone_koppen_5
    )
  ) +
  coord_fixed(
    ylim = c(min(for_analysis$lat), 
             max(for_analysis$lat)),
    xlim = c(min(for_analysis$long), 
             max(for_analysis$long))
  ) +
  labs(
    x = expression(
      paste(
        'Longitude ', (degree ~ E)
      )
    ),
    y = expression(
      paste(
        'Latitude ', (degree ~ N)
      )
    ),
    colour = "Climate zone") +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 50)
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 15)
  ) + 
  theme_classic() +
  borders(
    colour = "black",
    linewidth = 0.15
  ) +
  ggtitle("Number of fossil pollen records = 1357") +
  geom_point(
    aes(
      colour = ecozone_koppen_5
    ),
    size = 1.5,
    alpha = 0.55
  ) +
  scale_colour_manual(values = palette_ecozone) + 
  
  guides(
    colour = guide_legend(nrow = 1)
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text = element_text(colour = "black",
                             size = 12),
    axis.title = element_text(colour = "black", 
                              size = 14),
    plot.margin = margin(0, 0, 0.1, 0, "cm")
  ) 

p2 <-
  data_to_plot %>%
  dplyr::mutate(
    data_type = ifelse(
      pollen_percentage == "FALSE",
      "Counts",
      "Percentages"),
    distribution = "Global"
  ) %>%
  ggplot(
    aes(
      x = fct_infreq(source_of_data_specific),
      fill = data_type)
  ) +
  geom_bar(
    aes(y = after_stat(count)
    )
  ) +
  scale_fill_manual(values = palette_matching[c(1,5)]) +
  labs(fill = "Data type") + 
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    vjust = 0.55,
    size = 3
    ) +
  theme_classic() +
  facet_wrap(~ distribution) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(color = "black",
                               size = 12,),
    axis.text.x = element_text(
      color = "black",
      size = 12,
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(size = 12, 
                              color = "black"),
    legend.position = c(0.7, 0.8),
    legend.background = element_rect(colour = "transparent",
                                     fill = "transparent"),
    legend.key.size = unit(0.50, "cm")
  )


p3 <-
  p2 +
  facet_wrap(~ region,
             scales = "free_y",
             ncol = 3
  ) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, 
                                  color = "black"
        )
  )

p4 <-
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(
    p2,
    p3,
    ncol = 2,
    widths = c(0.25, 0.75),
    left = grid::textGrob(
      "Number of pollen records",
      rot = 90,
      hjust = 0,
      gp = grid::gpar(fontsize = 16)
    ),
    bottom = grid::textGrob("Data source",
                            gp = grid::gpar(fontsize = 16)
                            )
    )
  )

final_plot <-
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(p1,
                           p4,
                           nrow = 2
                           )
    )

#ggsave(final_plot,
#       file = paste("Data_summary_outputs/Figure/",
#                    "Datasets_filtered_ex_africa_210823.tiff",
#                    sep = ""),
#       dpi = 400,
#       compress = "lzw",
#       width = 20
#       height = 20,
#       unit = "cm")


