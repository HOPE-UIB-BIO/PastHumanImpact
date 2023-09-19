########################################################
## VISUALISATION
## FIGURE 2: RESULTS H1 DETAILS ON HUMANS
########################################################

# Import tables for plotting

source("R/working_scripts/Results_script.R")


# Define colour palette
# Ecozones
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

# Predictors
palette_pred <- c(human = "#663333", 
                  climate = "#BBBBBB") 


# Density figures for full distribution of variance
data_dist <- 
  data_spatial_vis %>%
  mutate(predictor = factor(predictor, 
                            levels = c("human", "climate", "time"))) %>%
  mutate(sel_classification = factor(sel_classification))  %>%
  pivot_longer(c(Unique_percent, Average.share_percent, Individual_percent), 
               names_to = "var_part", 
               values_to = "percentage") %>%
  mutate(var_part = factor(var_part, 
                           levels = c("Unique_percent", 
                                      "Average.share_percent",
                                      "Individual_percent")))


density_fig <-
  data_dist %>%
  group_by(region) %>%
  group_map(~ ggplot(
    data = .x, 
    aes(x = percentage)
    ) +
  geom_density(aes(
    y = after_stat(count),
    col = var_part,
    fill = var_part
    ),
    alpha = 0.4
  ) +
    scale_colour_manual(
      values = c("Unique_percent" = "maroon4",
                 "Average.share_percent" = "grey70",
                 "Individual_percent" = "grey30")
    ) +
    scale_fill_manual(
      values = c("Unique_percent" = "maroon4",
                 "Average.share_percent" = "grey70",
                 "Individual_percent" = "grey30")
    ) +
  facet_wrap(~predictor, 
             ncol = 3, 
             scales = "free") +
  labs(x = NULL,
       y = NULL)
  )

names(density_fig) <- data_dist$region %>% unique() 


### get constrained spd scores

constrained_scores <- 
  output_h1_spatial %>%
  mutate(constrained_scores = 
           purrr::map(data_merge,
                      .f = possibly(get_scores_constrained_spd,
                                    otherwise = NA_real_))) %>%
  inner_join(data_meta %>% 
               dplyr::select(dataset_id, 
                             sel_classification, 
                             region), 
             by = "dataset_id") %>%
  dplyr::select(dataset_id, 
                region, 
                sel_classification, 
                constrained_scores) %>%
  dplyr::mutate(scores = 
                  purrr::map(constrained_scores, 
                             pluck("scores"))) %>%
  unnest(scores) %>%
  dplyr::mutate(adjr2 = purrr::map(
    constrained_scores,
    .f = . %>% pluck("adjr2"))) %>%
  unnest(adjr2)


ecosystem_prop_scores <- 
  constrained_scores  %>%
  filter(adjr2 > 0.1) %>%
  dplyr::select(dataset_id:sel_classification, CAP1, age) %>%
  group_by(region) %>%
  group_map(~ ggplot(data = .x, aes(
    x = age, 
    y = CAP1, 
    group = dataset_id)
    ) +
  geom_line(aes(
    col = sel_classification),
    alpha = 0.3, 
    linewidth = 1) +
  scale_colour_manual(
    values = palette_ecozones
    ) +
    scale_y_continuous(limits = c(0, 5)) +
  theme_bw()+
  theme(
    strip.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(),
    plot.background = element_blank(), 
    plot.margin = grid::unit(c(0.2,  0.2, 0.2, 0), "mm"),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    axis.title = element_text(size = 8)
    
  ) +
  labs(
    y = "Scores",
    x = "Age BP")
  )

names(ecosystem_prop_scores) <- constrained_scores$region %>% unique()



theme_remove_labels <- theme(
  
  axis.text.x = element_blank()
)
#combine figures
combined_detail_h1 <- 
  cowplot::plot_grid(
  cowplot::plot_grid(density_fig$`North America`,
                  ecosystem_prop_scores$`North America`+
                    theme(axis.title.x = element_blank(),
                          axis.text.x = element_blank()),
                   nrow = 1
                   ),
  cowplot::plot_grid(density_fig$`Latin America`,
                     ecosystem_prop_scores$`Latin America` +
                       theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank()),
                     nrow = 1
  ),
  cowplot::plot_grid(density_fig$`Europe`,
                     ecosystem_prop_scores$`Europe`+
                       theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank()),
                     nrow = 1
  ),
  cowplot::plot_grid(density_fig$`Asia`,
                     ecosystem_prop_scores$`Asia` +
                       theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank()),
                     nrow = 1
  ),
  cowplot::plot_grid(density_fig$`Oceania`,
                     ecosystem_prop_scores$`Oceania`,
                     nrow = 1
  ),
ncol = 1)

