plot_data_events <- function(data_source_events,
                             select_region = NULL,
                             data_raw = FALSE) {
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

  data_merge <-
    data_source_events %>%
    tidyr::unnest(data_to_fit) %>%
    dplyr::inner_join(
      data_meta_work,
      by = "dataset_id"
    )

  if (
    isTRUE(data_raw)
  ) {
    data_merge <-
      data_merge %>%
      tidyr::pivot_longer(
        bi:ei,
        names_to = "var_name",
        values_to = "value"
      ) %>%
      dplyr::mutate(value = round(value))
  }

  if (
    !is.null(select_region)
  ) {
    data_sub <-
      data_merge %>%
      dplyr::filter(region == select_region)
  } else {
    data_sub <-
      data_merge
  }

  data_work <-
    data_sub %>%
    dplyr::group_by(
      region, sel_classification, var_name
    ) %>%
    tidyr::nest(data_to_fit = -c(region, sel_classification, var_name)) %>%
    dplyr::ungroup()

  data_work_mod <-
    data_work %>%
    dplyr::mutate(
      mod = purrr::map(
        .progress = "fiting general trend",
        .x = data_to_fit,
        .f = purrr::possibly(
          ~ REcopol::fit_custom_gam(
            data = .x,
            x_var = "age",
            y_var = "value",
            error_family = "stats::binomial(link = 'logit')"
          )
        ),
        otherwise = NA_character_
      )
    )

  data_work_pred <-
    data_work_mod %>%
    dplyr::filter(
      purrr::map_lgl(mod, ~ "gam" %in% class(.x))
    ) %>%
    dplyr::mutate(
      data_pred = purrr::map(
        progress = "predicting models",
        .x = mod,
        .f = purrr::possibly(
          ~ REcopol::predic_model(
            model_source = .x,
            data_source = tibble::tibble(
              age = seq(from = 0, to = 12e3, length.out = 100)
            )
          )
        )
      )
    )

  data_to_plot <-
    data_work_pred %>%
    tidyr::unnest(data_pred) %>%
    dplyr::select(
      region,
      sel_classification,
      var_name,
      age,
      fit
    ) %>%
    dplyr::mutate(
      var_name = dplyr::case_when(
        .default = "no impact",
        var_name == "bi" ~ "no impact",
        var_name == "fi" ~ "first impact",
        var_name == "ei" ~ "emerging impact",
        var_name == "ec" ~ "extensive clearince",
        var_name == "cc" ~ "complete clearince",
        var_name == "fc" ~ "first cultivation",
        var_name == "es" ~ "europiean settlement",
        var_name == "weak" ~ "weak impact",
        var_name == "medium" ~ "medium impact",
        var_name == "strong" ~ "strong impact"
      ),
      var_name = factor(
        var_name,
        levels = c(
          "no impact",
          "first impact",
          "emerging impact",
          "extensive clearince",
          "complete clearince",
          "first cultivation",
          "europiean settlement",
          "weak impact",
          "medium impact",
          "strong impact"
        )
      )
    )

  set_palette <-
    c(
      "grey60",
      "#c99000",
      "#a17400",
      "#7b5800",
      "#573e00",
      "#00c92b",
      "#c9009e",
      "#9b541b",
      "#5d261a",
      "#1f0000"
    ) %>%
    rlang::set_names(
      levels(data_to_plot$var_name)
    )

  fig <-
    data_to_plot %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = age,
        y = fit,
        col = var_name
      )
    ) +
    ggplot2::facet_grid(region ~ sel_classification) +
    # ggplot2::scale_colour_hue(c = 50, l = 50, h = c(30, 300)) +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      limits = c(12e3, 0),
      breaks = seq(12e3, 0, by = -2e3),
      labels = seq(12, 0, by = -2)
    ) +
    ggplot2::scale_fill_manual(
      values = set_palette
    ) +
    ggplot2::scale_color_manual(
      values = set_palette
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = text_size,
        hjust = 0.01
      ),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = ifelse(is.null(select_region), "", select_region),
      x = "Age (ka cal yr BP)"
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = 0,
        ymax = fit,
        fill = var_name
      ),
      alpha = 0.3
    )


  return(fig)
}
