
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
# 1. Summary tables -----
#----------------------------------------------------------#
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
# 3. Figures -----
#----------------------------------------------------------# 
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

  # alternative 1: unique partition and predictor importance  
wmean_importance_fig <-
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
        importance_type == "ratio_unique_wmean"
      ),
      mapping = ggplot2::aes(
        x = ratio,
        y = predictor,
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
  ggplot2::geom_bar(
    data = . %>%
      dplyr::filter(
        importance_type == "ratio_ind_wmean"
      ),
      mapping = ggplot2::aes(
        x = ratio,
        y = predictor,
        fill = climatezone
      ),
    stat = "identity",
    width = .6,
    alpha = 0.4,
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
  )+
  ggplot2::scale_x_continuous(limits = c(0, 1.2)) +
  
  facet_wrap(
  ~region, nrow = 1 
  )

ggarrange(wmean_importance_fig,
          fig_m2_change_region,
          nrow = 2
          )

# extract pcoa scores for plotting
get_pcoa_sc <- function(pcoa_res){
  table <- pcoa_res$points %>%
    data.frame() %>%
    rownames_to_column("age")
  return(table)
}

# plot m2 trajectory changes
plot_tracjectory_pcoa_scores <- 
  function(data_source){
  fig <- data_source %>%
    ggplot( aes(x = X1, y = X2, col = climatezone)) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.1) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.1) +
    scale_y_continuous(limits = c(-0.75, 0.5),
                       breaks = seq(-0.75, 0.5, by = 0.25)) +
    scale_x_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, by =0.3)) + 
    scale_color_manual(
      values = palette_ecozones,
      drop = FALSE) + 
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
      axis.title.y = ggplot2::element_text(size = 6),
      axis.text.x = ggplot2::element_text(size = 6, angle = 60),
      axis.text.y = ggplot2::element_text(size = 6),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    )  +
    geom_point() +
    geom_path(lineend = "round", 
              linejoin = "bevel",
              arrow = arrow(length = unit(0.1, "inches"), 
                            ends = "last",
                            type = "open"),
              linewidth = 0.75) +
    labs(x = "", y = "") 
  
  return(fig)
}

# prepare data
m2_pcoa_scores  <- 
  data_m2_filtered %>%
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
  mutate(pcoa_scores = 
           purrr::map(
             .x = PCoA,
             .f = get_pcoa_sc
           )) %>%
  dplyr::select(region, climatezone, ecozone_koppen_5, pcoa_scores) %>%
  unnest(cols = pcoa_scores)  %>%
  nest(data_to_plot = -c(region, ecozone_koppen_5))

m2_pcoa_scores <-
  m2_pcoa_scores %>%
  mutate(m2_pcoa_fig = purrr::map(
    .x = data_to_plot,
    .f = plot_tracjectory_pcoa_scores)
    )
         

# arrange plot order
m2_pcoa_scores <- 
  m2_pcoa_scores %>%
  arrange(factor(region, levels = vec_regions),
          factor(ecozone_koppen_5, levels = vec_climate_5)) 
  
# plotgrid
  
fig_trajectory_m2 <- 
  m2_pcoa_scores$m2_pcoa_fig %>%
  cowplot::plot_grid(plotlist = .,
                     ncol = 5,
                     nrow = 4,
                     byrow = FALSE) 
  
# combine 
  cowplot::plot_grid(
    wmean_importance_fig,
    fig_trajectory_m2,
    nrow = 2
  )
  
ggsave(file = "importance_h2.png",
       plot = wmean_importance_fig,
       bg = "white",
       width = 297,
       height = 100,
       units = "mm")  

ggsave("fig_trajectory_m2.png",
       plot = fig_trajectory_m2,
       width = 297,
       height = 210,
        units = "mm",
       bg = "white")






##########################################################################
# check dbRDA  
output_h2$data_response_dist[[1]] %>% as.dist()
output_h2$data_merge[[1]]
  
test <- dbrda(as.dist(output_h2$data_response_dist[[7]])~spd + temp_annual + temp_cold + prec_summer + prec_win,
              output_h2$data_merge[[7]][,-1])

plot(test)
  