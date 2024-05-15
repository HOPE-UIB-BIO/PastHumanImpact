
#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
#
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#
# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)


output_h2 <-
targets::tar_read(
 name = "output_hvar_h2_spd",
store = paste0(
 data_storage_path,
"_targets_data/analyses_h2"
 )
 )

data_m2_filtered <-
targets::tar_read(
     name = "data_m2_filtered",
     store = paste0(
      data_storage_path,
      "_targets_data/analyses_h2"
      )
    )
#----------------------------------------------------------#
# 1. Run and get scores dbRDA for visualisation -----
#----------------------------------------------------------#

# add dbrda model to output_h2
output_h2  <- 
  output_h2 %>%
  mutate(mod_dbrda = 
           purrr::map2(
             .x = data_response_dist,
             .y = data_merge,
             .f = ~run_dbrda(.x, .y)
           )) %>%
  mutate(scores_dbrda = 
           purrr::map(
             .x = mod_dbrda,
             .f = ~get_scores_dbrda(.x)
           )

  )

data_to_plot_trajectory <-
  output_h2 %>%
  dplyr::select(
    region,
    climatezone,
    scores_dbrda) %>%
  left_join(data_meta %>% 
              dplyr::select(region, 
                            climatezone, 
                            ecozone_koppen_5) %>%
              distinct()) %>%
  dplyr::mutate(
    region = factor(region,
                    levels = vec_regions # [config criteria]
    ),
    ecozone_koppen_5 = factor(
      ecozone_koppen_5,
      levels = vec_climate_5 # [config criteria]
    )
  ) %>%
  unnest(scores_dbrda)

#----------------------------------------------------------#
# 2. Summary tables -----
#----------------------------------------------------------#
# all data  
table_h2 <-  
   output_h2 %>%
    dplyr::mutate(
       summary_table = purrr::map(
          .x = varhp,
            .f = ~ .x %>%
              purrr::pluck("summary_table")
          )
      ) %>%
     tidyr::unnest(summary_table) %>%
     dplyr::select(-c(data_merge, data_response_dist, varhp)) %>%
     dplyr::mutate(
      dplyr::across(
       .cols = Unique,
       .fns = ~ replace(., .x < 0, 0.0001)
      )
   ) %>% # negative variances can be ignored  
   janitor::clean_names() %>%
    group_by(
       region, climatezone
     ) %>%
   mutate(sum_importance = sum(individual),
    ratio_unique = unique/sum_importance,
    ratio_ind = individual/sum_importance) %>%
   ungroup()

# reshape long formate  
  summary_h2_long <-
    table_h2 %>%
    dplyr::group_by(
      region,
      climatezone,
      predictor
    ) %>% 
    #summarise by model weight
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(
          c("ratio_unique",
            "ratio_ind")
        ),
        list(
          wmean = ~ weighted.mean(
            x = .x,
            w = sum_importance,
            na.rm = TRUE)
        )
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("ratio"),
      names_to = "importance_type",
      values_to = "ratio"
    ) %>%
    dplyr::mutate(
      region = factor(region,
                      levels = vec_regions # [config criteria]
      ),
      climatezone = recode(climatezone, "Polar" = "Polar",
                           "Cold_Without_dry_season_Very_Cold_Summer" ="Cold_Very_Cold_Summer",
                           "Cold_Without_dry_season_Cold_Summer" = "Cold_Cold_Summer",
                           "Cold_Without_dry_season_Warm_Summer" = "Cold_Warm_Summer",
                           "Cold_Without_dry_season_Hot_Summer" = "Cold_Hot_Summer",
                           "Cold_Dry_Winter" = "Cold_Dry_Winter",
                           "Cold_Dry_Summer" = "Cold_Dry_Summer",
                           "Temperate_Without_dry_season" = "Temperate",
                           "Temperate_Dry_Winter" = "Temperate_Dry_Winter",
                           "Temperate_Dry_Summer" = "Temperate_Dry_Summer",
                           "Tropical" = "Tropical",
                           "Arid" = "Arid"),
      climatezone = factor(
        climatezone,
        levels = levels(data_climate_zones$climatezone)# [config criteria]
      )
    ) 
  



#----------------------------------------------------------#
# 3. Ratio of predictor importance -----
#----------------------------------------------------------#   


pred_importance_fig <-
  summary_h2_long %>%
  mutate(
    predictor = factor(
      predictor,
      levels = c("human", "climate")
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    data = . %>%
      dplyr::filter(
        importance_type == "ratio_ind_wmean"
      ),
      mapping = ggplot2::aes(
        y = ratio,
        x = climatezone,
        fill = predictor
      ),
    stat = "identity",
    width = 0.9,
    alpha = 1,
    position = "stack",
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = palette_predictors,
    drop = FALSE
  ) +
  ggplot2::theme(
    aspect.ratio = 1/3,
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    #strip.text.y = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 8, angle = 60, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 6),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  )+
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(
  ~region, ncol = 1
  ) +
    labs(x = "")

pred_importance_fig



#----------------------------------------------------------#
# 4. Trajectory plot -----
#----------------------------------------------------------# 



figure_trajecotries <- 
  data_to_plot_trajectory %>%
     ggplot() +
     ggplot2::geom_segment(
       data = . %>%
         dplyr::filter(score %in% c("biplot")),
       mapping = ggplot2::aes(
         x = 0,
         y = 0,
         xend = dbRDA1,
         yend = dbRDA2),
       arrow = arrow(length = unit(0.03, "npc")),
       col = "black"
     ) +
     geom_text(
       data = . %>%
         dplyr::filter(score %in% c("biplot")),
       mapping = ggplot2::aes(
         x = dbRDA1*1.2,
         y = dbRDA2*1.2,
         label = label),
       size = 3,
       col = "black"
     ) +
     geom_path(
       data = . %>%  
         dplyr::filter(score %in% c("sites")), 
       mapping = ggplot2::aes(
         x = dbRDA1,
         y = dbRDA2,
         col = climatezone
       ),
       lineend = "round",
       linejoin = "bevel",
       linewidth = 0.5,
       arrow = arrow(
         length = unit(0.1, "inches"),
         ends = "first",
         type = "open")) +
  geom_point(
    data = . %>%  
      dplyr::filter(score %in% c("sites")), 
    mapping = ggplot2::aes(
      x = dbRDA1,
      y = dbRDA2,
      col = climatezone
    ),
    size = 0.5)+
     geom_text(
       data = . %>%
         dplyr::filter(score %in% c("sites")) %>%
         mutate(label = as.character(as.numeric(label)/1000)),
       mapping = ggplot2::aes(
         x = dbRDA1,
         y = dbRDA2,
         label = label),
       size = 3,
       vjust = 1.5,
       col = "black"
     ) +
     geom_vline(
       xintercept = 0,
       linetype = 2, 
       linewidth = 0.1) +
     geom_hline(
       yintercept = 0, 
       linetype = 2, 
       linewidth = 0.1) +
     coord_fixed(xlim = c(-2,2) ) +
     scale_color_manual(
       values = palette_ecozones,
       drop = FALSE) +
     ggplot2::theme(
       legend.position = "none",
       panel.background = ggplot2::element_blank(),
       strip.background = ggplot2::element_blank(),
       #strip.text.y = ggplot2::element_blank(),
       # strip.text.x = ggplot2::element_blank(),
       panel.grid.minor = ggplot2::element_blank(),
       plot.background = ggplot2::element_rect(
         fill = "transparent",
         color = NA
       ),
       axis.title.x = ggplot2::element_text(size = 8),
       axis.title.y = ggplot2::element_text(size = 8),
       axis.text.x = ggplot2::element_text(size = 8, angle = 60),
       axis.text.y = ggplot2::element_text(size = 8),
       plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
     )  +
     ggplot2::facet_wrap(
       ~climatezone+ region,
     ) +
     labs(x = "dbRDA 1", y = "dbRDA 2")
 
    








#----------------------------------------------------------#
# 7. Combine plots & save -----
#----------------------------------------------------------#
cowplot::plot_grid(
  pred_importance_fig,
  main_trajectory_plot,
  ncol = 2,
  rel_heights = c(0.5, 2)
)



 figure4 <- 
   cowplot::ggdraw() +
  cowplot::draw_plot(
   pred_importance_fig,
    x = 0.7,
    y = 0,
    width = 0.2,
    height = 0.95
  ) +
  cowplot::draw_plot(
    main_trajectory_plot,
    x = 0,
    y = 0,
    width = 0.78,
    height = 1
  )  

 # save predictor importance
 purrr::walk(
   .x = c("png", "pdf"),
   .f = ~ ggplot2::ggsave(
     paste(
       here::here("Outputs/Figure4_predictor_importance"),
       .x,
       sep = "."
     ),
     plot = pred_i,
     width = image_width_vec["1col"], # [config criteria]
     height = 160,
     units = image_units, # [config criteria]
     bg = "white"
   )
 )  

# save trajectories
purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/figure4_h2"),
      .x,
      sep = "."
    ),
    plot = figure4,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)




##########################################################################

  