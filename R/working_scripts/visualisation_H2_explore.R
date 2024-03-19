
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
# 1. Add model of dbRDA for visualisation -----
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
    ) 
  


  
#----------------------------------------------------------#
# 3. Plot m2 change between consequtive time -----
#----------------------------------------------------------# 
 
# 3.1. Figure: Change in m2 between consecutive time  
   fig_m2_change_region <-
    data_m2_filtered %>%
    dplyr::select(
      m2_time_df,
      region,
      climatezone
    ) %>%
  
    tidyr::unnest(cols = c(m2_time_df)) %>%
    tidyr::complete(
      time,
      tidyr::nesting(climatezone, region)
    ) %>%
    dplyr::inner_join(
      data_meta %>%
        dplyr::select(region,climatezone, ecozone_koppen_5) %>%
        dplyr::distinct(),
      by = c("region", "climatezone")
    ) %>%
    dplyr::mutate(
      region = factor(region,
                      levels = vec_regions # [config criteria]
      ),
      ecozone_koppen_5 = factor(
        ecozone_koppen_5,
        levels = vec_climate_5 # [config criteria]
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = as.numeric(time),
        y = delta_m2,
        col = climatezone,
        fill = climatezone
      )
    ) +
    ggplot2::facet_grid(
      ~ region
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      limits = c(8.5e3, 500),
      breaks = c(seq(8.5e3, 500, by = -2e3)),
      labels = c(seq(8.5, 0.5, by = -2))
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(
      values = palette_ecozones,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = palette_ecozones,
      drop = FALSE
    ) +
    ggplot2::theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      panel.grid.major = ggplot2::element_line(
        color = "grey90",
        linewidth = 0.1
      ),
      axis.title.x = ggplot2::element_text(size = 6),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 6, angle = 60),
      axis.text.y = ggplot2::element_text(size = 6),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    ) +
    ggplot2::labs(
      x = "Age (ka cal yr BP)",
      y = "change in m2"
    ) +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      linewidth = 0.1,
      lty = 2,
      se = FALSE
    ) +
    ggplot2::geom_smooth(
      method = "gam",
      se = FALSE,
      formula = y ~ s(x, bs = "tp", k = 10),
      method.args = list(
        family =
          mgcv::betar(link = "logit")
      ),
      linewidth = 0.2
    )

#----------------------------------------------------------#
# 4. Ratio of predictor importance -----
#----------------------------------------------------------#   

pred_importance_fig <-
  summary_h2_long %>%
  dplyr::mutate(
    region = factor(region,
                    levels = vec_regions # [config criteria]
    )
  )%>%
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    data = . %>%
      dplyr::filter(
        importance_type == "ratio_ind_wmean"
      ),
      mapping = ggplot2::aes(
        y = ratio,
        x = predictor,
        fill = climatezone
      ),
    stat = "identity",
    width = .6,
    alpha = 1,
    position = ggplot2::position_dodge2(
      width = 0.8,
      preserve = "single"
    ),
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones,
    drop = FALSE
  ) +
  ggplot2::theme(
    aspect.ratio = 1,
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 8),
    axis.text.y = ggplot2::element_text(size = 6),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  )+
  ggplot2::scale_y_continuous(limits = c(0, 1.2)) +
  facet_grid(
  ~region
  ) +
    labs(x = "")

pred_importance_fig



#----------------------------------------------------------#
# 5. Trajectory plot -----
#----------------------------------------------------------# 


main_trajectory_plot <-
  data_to_plot_trajectory %>%
  ggplot() +
  # ggplot2::geom_segment(
  #   data = . %>%
  #     dplyr::filter(score %in% c("biplot")),
  #   mapping = ggplot2::aes(
  #     x = 0,
  #     y = 0,
  #     xend = dbRDA1,
  #     yend = dbRDA2),
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   col = "black"
  # ) +
  # geom_text(
  #   data = . %>%
  #     dplyr::filter(score %in% c("biplot")),
  #   mapping = ggplot2::aes(
  #     x = dbRDA1*1.2,
  #     y = dbRDA2*1.2,
  #     col = climatezone,
  #     label = label),
  #   size = 3
  # ) +
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
      ends = "last",
      type = "open")) +
  geom_vline(
    xintercept = 0,
    linetype = 2, 
    linewidth = 0.1) +
  geom_hline(
    yintercept = 0, 
    linetype = 2, 
    linewidth = 0.1) +
  # scale_y_continuous(
  #   limits = c(-2.0, 2.0),
  #   breaks = seq(-2.0, 2.0, by = 0.5)) +
  # scale_x_continuous(
  #   limits = c(-2, 1.5),
  #   breaks = seq(-2, 1.5, by = 0.5)) +
  scale_color_manual(
    values = palette_ecozones,
    drop = FALSE) +
  ggplot2::theme(
    aspect.ratio = 1,
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    axis.title.x = ggplot2::element_text(size = 6),
    axis.title.y = ggplot2::element_text(size = 6),
    axis.text.x = ggplot2::element_text(size = 6, angle = 60),
    axis.text.y = ggplot2::element_text(size = 6),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  )  +
  ggplot2::facet_grid(
    ecozone_koppen_5 ~ region,  
  ) +
  labs(x = "", y = "")
    

main_trajectory_plot


         
# # insert of importance
# importance_inset_plot <- 
#   summary_h2_long %>%
#   filter(importance_type == "ratio_ind_wmean") %>%
#   nest(data_for_inset = -c(region, climatezone)) %>%
#   left_join(data_meta %>% 
#               dplyr::select(region, climatezone, ecozone_koppen_5) %>%
#               distinct()) %>%
#   dplyr::mutate(
#     region = factor(region,
#                     levels = vec_regions # [config criteria]
#     ),
#     ecozone_koppen_5 = factor(
#       ecozone_koppen_5,
#       levels = vec_climate_5 # [config criteria]
#     )
#   ) %>%
#   unnest(data_for_inset) %>%
#   group_by(region, 
#            ecozone_koppen_5) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_bar(
#     mapping = ggplot2::aes(
#       x = ratio,
#       y = predictor,
#       fill = climatezone
#     ),
#     stat = "identity",
#     width = .3,
#     alpha = 1,
#     position = ggplot2::position_dodge2(
#       width = 0.3,
#       preserve = "single"
#     ),
#     show.legend = FALSE
#   ) +
#   ggplot2::scale_fill_manual(
#     values = palette_ecozones,
#     drop = FALSE
#   ) +
#   ggplot2::theme(
#     aspect.ratio = 1,
#     legend.position = "none",
#     panel.background = ggplot2::element_blank(),
#     strip.background = ggplot2::element_blank(),
#     strip.text.y = ggplot2::element_blank(),
#     panel.grid.minor = ggplot2::element_blank(),
#     plot.background = ggplot2::element_rect(
#       fill = "transparent",
#       color = NA
#     ),
#     panel.grid.major = ggplot2::element_line(
#       color = "grey90",
#       linewidth = 0.1
#     ),
#     axis.title.x = ggplot2::element_text(size = 6),
#     axis.title.y = ggplot2::element_blank(),
#     axis.text.x = ggplot2::element_text(size = 6, angle = 60),
#     axis.text.y = ggplot2::element_text(size = 6),
#     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
#   ) +
# labs(y = "", x = "") 
#  
# 
# importance_inset_plot

 

#----------------------------------------------------------#
# 7. Combine plots & save -----
#----------------------------------------------------------#

combine_h2 <-
  cowplot::ggdraw() +
  cowplot::draw_plot(
   pred_importance_fig,
    x = 0,
    y = 0.78,
    width = 1,
    height = 0.22
  ) +
  cowplot::draw_plot(
    main_trajectory_plot,
    x = 0,
    y = 0,
    width = 1.0,
    height = 0.78
  )  

  


purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combine_h2"),
      .x,
      sep = "."
    ),
    plot = combine_h2,
    width = image_width_vec["2col"], # [config criteria]
    height = 165,
    units = image_units, # [config criteria]
    bg = "white"
  )
)




##########################################################################

  