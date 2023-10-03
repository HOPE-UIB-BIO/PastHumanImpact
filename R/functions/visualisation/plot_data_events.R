plot_data_events <- function(data_source_events,
                             select_region = NULL,
                             data_raw = TRUE) {
  data_meta_work <-
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
    ) %>%
    dplyr::mutate(
      sel_classification = as.factor(sel_classification)
    ) %>%
    dplyr::full_join(
      data_climate_zones, # [config criteria]
      .,
      by = "sel_classification"
    ) %>%
    dplyr::mutate(
      region = factor(region,
        levels = vec_regions # [config criteria]
      )
    ) %>%
    dplyr::filter(
      region != "Africa"
    ) %>%
    dplyr::select(
      region,
      sel_classification,
      dataset_id,
      long, lat
    )

  data_work <-
    data_source_events %>%
    tidyr::unnest(data_to_fit) %>%
    dplyr::inner_join(
      data_meta_work,
      by = "dataset_id"
    )

  if (
    isTRUE(data_raw)
  ) {
    data_to_plot <-
      data_work %>%
      dplyr::filter(region == select_region)
  } else {
    data_to_plot <-
      data_work %>%
      tidyr::pivot_longer(
        bi:ei,
        names_to = "var_name",
        values_to = "value"
      ) %>%
      dplyr::filter(region == select_region) %>%
      dplyr::mutate(value = round(value))
  }

  fig <-
    data_to_plot %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = age,
        y = value,
        col = var_name
      )
    ) +
    ggplot2::facet_wrap(~sel_classification) +
    ggplot2::scale_colour_hue(c = 50, l = 50, h = c(30, 300)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = select_region, x = "") +
    ggplot2::geom_smooth(
      method = "gam",
      se = FALSE,
      formula = y ~ s(x, bs = "cs"),
      method.args = list(
        family =
          stats::binomial(link = "logit")
      )
    )


  return(fig)
}
