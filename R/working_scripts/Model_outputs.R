#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#                 FIGURE 1: RESULTS H1
#
#                   V. Felde, O. Mottl
#                         2023
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

output_h1_spatial <-
  targets::tar_read(
    name = "output_hvar_spatial",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

output_hvar_h2 <-
  targets::tar_read(
    name = "output_hvar_h2",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  )

data_meta <-
  targets::tar_read(
    name = "data_meta",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  ) %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  )

#----------------------------------------------------------#
# 2. H1 results -----
#----------------------------------------------------------#

data_work_h1 <-
  output_h1_spatial %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ purrr::pluck(.x, "summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::mutate(
    total_variance = purrr::map_dbl(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("varhp_output", "Total_explained_variation")
    )
  ) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  dplyr::rename(
    p_value = `Pr(>I)`,
    Individual_percent = `I.perc(%)`
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique:Individual_percent,
      .fns = ~ replace(., .x < 0, 0)
    )
  ) %>% # negative variances can be ignored
  dplyr::mutate(
    Individual_percent = Individual / total_variance * 100
  ) %>% # recalculate individual percent
  dplyr::mutate(
    Unique_percent = Unique / total_variance * 100,
    Average.share_percent = Average.share / total_variance * 100
  ) %>%
  janitor::clean_names()

data_predictor_h1_variance <-
  purrr::map(
    .x = c("human", "climate", "time"),
    .f = ~ {
      sel_pred <- .x

      data_work_h1 %>%
        dplyr::filter(predictor == sel_pred) %>%
        dplyr::select(
          dataset_id,
          total_variance,
          predictor_unique = unique,
          predictor_avg_share = average_share,
          predictor_total = individual
        ) %>%
        dplyr::rename_with(
          .cols = c(
            "predictor_unique",
            "predictor_avg_share",
            "predictor_total"
          ),
          .fn = ~ paste0(sel_pred, "_", .x)
        )
    }
  ) %>%
  purrr::reduce(
    .f = dplyr::full_join,
    by = dplyr::join_by(dataset_id, total_variance)
  ) %>%
  dplyr::distinct()

data_predictor_h1_percentage <-
  purrr::map(
    .x = c("human", "climate", "time"),
    .f = ~ {
      sel_pred <- .x

      data_work_h1 %>%
        dplyr::filter(predictor == sel_pred) %>%
        dplyr::select(
          dataset_id,
          predictor_unique_percent = unique_percent,
          predictor_avg_share_percent = average_share_percent,
          predictor_total_percent = individual_percent
        ) %>%
        dplyr::rename_with(
          .cols = c(
            "predictor_unique_percent",
            "predictor_avg_share_percent",
            "predictor_total_percent"
          ),
          .fn = ~ paste0(sel_pred, "_", .x)
        )
    }
  ) %>%
  purrr::reduce(
    .f = dplyr::full_join,
    by = dplyr::join_by(dataset_id)
  ) %>%
  dplyr::distinct()

data_filter_out_h1 <-
  data_work_h1 %>%
  dplyr::mutate(
    filter_out = dplyr::if_else(
      total_variance < 0, TRUE, FALSE
    ),
    filter_out = dplyr::if_else(
      total_variance < quantile(
        total_variance,
        probs = 0.050,
        na.rm = TRUE
      ), TRUE, filter_out
    )
  ) %>%
  dplyr::mutate(
    filter_out = dplyr::if_else(
      individual_percent > 100 |
        unique_percent > 100 |
        average_share_percent > 100,
      TRUE, filter_out
    ),
    filter_out = dplyr::if_else(
      individual_percent < 0 |
        unique_percent < 0 |
        average_share_percent < 0,
      TRUE, filter_out
    )
  ) %>%
  dplyr::group_by(dataset_id) %>%
  dplyr::summarise(
    .groups = "drop",
    filter_out = any(filter_out)
  )

res_models_table_h1 <-
  data_meta %>%
  dplyr::select(
    region,
    climate_zone = sel_classification,
    long,
    lat,
    dataset_id
  ) %>%
  dplyr::arrange(region, climate_zone, dataset_id) %>%
  dplyr::filter(region != "Africa") %>%
  dplyr::distinct() %>%
  dplyr::inner_join(
    data_filter_out_h1,
    by = "dataset_id"
  ) %>%
  dplyr::left_join(
    data_predictor_h1_variance,
    by = "dataset_id"
  ) %>%
  dplyr::left_join(
    data_predictor_h1_percentage,
    by = "dataset_id"
  )

#----------------------------------------------------------#
# 3. H2 results -----
#----------------------------------------------------------#

data_work_h2 <-
  output_hvar_h2 %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::mutate(
    total_variance = purrr::map_dbl(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("varhp_output") %>%
        purrr::pluck("Total_explained_variation")
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique:`I.perc(%)`,
      .fns = ~ replace(., .x < 0, 0)
    )
  ) %>%
  dplyr::mutate(
    Individual_percent = Individual / total_variance * 100
  ) %>%
  # negative variances can be ignored
  dplyr::mutate(
    Unique_percent = Unique / total_variance * 100,
    Average.share_percent = Average.share / total_variance * 100
  ) %>%
  dplyr::select(-c(data_merge, varhp, responce_dist)) %>%
  dplyr::ungroup() %>%
  janitor::clean_names()


data_predictor_h2_variance <-
  purrr::map(
    .x = c("human", "climate", "time"),
    .f = ~ {
      sel_pred <- .x

      data_work_h2 %>%
        dplyr::filter(predictor == sel_pred) %>%
        dplyr::select(
          region, group,
          total_variance,
          predictor_unique = unique,
          predictor_avg_share = average_share,
          predictor_total = individual
        ) %>%
        dplyr::rename_with(
          .cols = c(
            "predictor_unique",
            "predictor_avg_share",
            "predictor_total"
          ),
          .fn = ~ paste0(sel_pred, "_", .x)
        )
    }
  ) %>%
  purrr::reduce(
    .f = dplyr::full_join,
    by = dplyr::join_by(region, group, total_variance)
  ) %>%
  dplyr::distinct()

data_predictor_h2_percentage <-
  purrr::map(
    .x = c("human", "climate", "time"),
    .f = ~ {
      sel_pred <- .x

      data_work_h2 %>%
        dplyr::filter(predictor == sel_pred) %>%
        dplyr::select(
          region, group,
          predictor_unique_percent = unique_percent,
          predictor_avg_share_percent = average_share_percent,
          predictor_total_percent = individual_percent
        ) %>%
        dplyr::rename_with(
          .cols = c(
            "predictor_unique_percent",
            "predictor_avg_share_percent",
            "predictor_total_percent"
          ),
          .fn = ~ paste0(sel_pred, "_", .x)
        )
    }
  ) %>%
  purrr::reduce(
    .f = dplyr::full_join,
    by = dplyr::join_by(region, group)
  ) %>%
  dplyr::distinct()

data_truncated_h2 <-
  data_work_h2 %>%
  dplyr::mutate(
    truncated = dplyr::if_else(
      individual_percent > 100 |
        unique_percent > 100 |
        average_share_percent > 100,
      TRUE, FALSE
    ),
    truncated = dplyr::if_else(
      individual_percent < 0 |
        unique_percent < 0 |
        average_share_percent < 0,
      TRUE, truncated
    )
  ) %>%
  dplyr::group_by(region, group) %>%
  dplyr::summarise(
    .groups = "drop",
    truncated = any(truncated)
  )

res_models_table_h2 <-
  data_meta %>%
  dplyr::select(
    region,
    climate_zone = sel_classification,
  ) %>%
  dplyr::arrange(region, climate_zone) %>%
  dplyr::filter(region != "Africa") %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    data_truncated_h2,
    by = dplyr::join_by(region, climate_zone == "group")
  ) %>%
  dplyr::left_join(
    data_predictor_h2_variance,
    by = dplyr::join_by(region, climate_zone == "group")
  ) %>%
  dplyr::left_join(
    data_predictor_h2_percentage,
    by = dplyr::join_by(region, climate_zone == "group")
  ) %>%
  dplyr::mutate(
    filter_out = dplyr::if_else(is.na(total_variance), TRUE, FALSE)
  ) %>%
  dplyr::relocate(
    region, climate_zone, filter_out, truncated
  )

#----------------------------------------------------------#
# 4. Plot distribution -----
#----------------------------------------------------------#
plot_dist_density <- function(data_source) {
  data_work <-
    data_source %>%
    dplyr::filter(
      filter_out == FALSE
    ) %>%
    dplyr::select(
      c(
        "region",
        dplyr::any_of(
          "truncated"
        ),
        dplyr::contains("percent")
      )
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        !dplyr::any_of(
          c(
            "truncated", "region"
          )
        )
      ),
      names_to = "predictor_full",
      values_to = "percentage"
    ) %>%
    dplyr::mutate(
      predictor = dplyr::case_when(
        stringr::str_detect(predictor_full, "human") ~ "human",
        stringr::str_detect(predictor_full, "climate") ~ "climate",
        stringr::str_detect(predictor_full, "time") ~ "time",
        .default = NA_character_
      ),
      var_par = dplyr::case_when(
        stringr::str_detect(predictor_full, "unique") ~ "unique_percent",
        stringr::str_detect(predictor_full, "avg_share") ~ "average_share_percent",
        stringr::str_detect(predictor_full, "total") ~ "individual_percent",
      )
    ) %>%
    dplyr::mutate(
      predictor = factor(
        predictor,
        levels = predictors_spatial_order # [config criteria]
      ),
      region = factor
      (region,
        levels = vec_regions # [config criteria]
      )
    )

  if (
    "truncated" %in% names(data_work)
  ) {
    data_work <-
      data_work %>%
      dplyr::mutate(
        percentage = dplyr::if_else(
          truncated == TRUE,
          100,
          percentage
        )
      )
  }

  fig <-
    data_work %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = percentage
      )
    ) +
    ggplot2::facet_grid(region ~ predictor) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(
      values = palette_predictors_parts, # [config criteria]
      labels = c("average share", "total", "unique")
    ) +
    ggplot2::scale_fill_manual(
      values = palette_predictors_parts, # [config criteria]
      labels = c("average share", "total", "unique")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size
      ),
      line = ggplot2::element_line(
        linewidth = line_size # [config criteria]
      ),
      legend.position = "bottom",
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      strip.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      col = "",
      fill = "",
      x = "Explained variability (%)",
      y = "N"
    ) +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        y = after_stat(count),
        col = var_par,
        fill = var_par
      ),
      alpha = 0.4,
      linewidth = 0.1
    )

  return(fig)
}

fig_h1_dist <-
  plot_dist_density(res_models_table_h1)

fig_h2_dist <-
  plot_dist_density(res_models_table_h2)

fig_dist_merge <-
  ggpubr::ggarrange(
    fig_h1_dist,
    fig_h2_dist,
    nrow = 1,
    ncol = 2,
    common.legend = TRUE,
    legend = "bottom",
    labels = c("H1", "H2")
  )

#----------------------------------------------------------#
# 5. Save -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  res_models_table_h1,
  file_name = "res_models_table_h1",
  dir = here::here(
    "Outputs"
  ),
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  res_models_table_h2,
  file_name = "res_models_table_h2",
  dir = here::here(
    "Outputs"
  ),
  prefered_format = "csv"
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Model_outputs_distribution"),
      .x,
      sep = "."
    ),
    plot = fig_dist_merge,
    width = image_width_vec["3col"], # [config criteria]
    height = 130,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
