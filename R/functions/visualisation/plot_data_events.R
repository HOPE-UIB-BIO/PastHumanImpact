plot_data_events <- function(data_source_events,
                             select_region = NULL,
                             data_raw = TRUE) {
  if (
    isTRUE(data_raw)
  ) {
    data <-
      data_source_events %>%
      tidyr::unnest(data_to_fit) %>%
      dplyr::inner_join(
        targets::tar_read(
          name = "data_meta",
          store = paste0(
            data_storage_path,
            "_targets_h1"
          )
        ) %>%
          dplyr::select(
            dataset_id,
            long,
            lat,
            region,
            ecozone_koppen_15
          ),
        by = "dataset_id"
      ) %>%
      dplyr::filter(region == select_region)
  } else {
    data <-
      data_source_events %>%
      tidyr::unnest(data) %>%
      dplyr::inner_join(
        targets::tar_read(
          name = "data_meta",
          store = paste0(
            data_storage_path,
            "_targets_h1"
          )
        ) %>%
          dplyr::select(
            dataset_id,
            long,
            lat,
            region,
            ecozone_koppen_15
          ),
        by = "dataset_id"
      ) %>%
      tidyr::pivot_longer(
        bi:ei,
        names_to = "var_name",
        values_to = "value"
      ) %>%
      dplyr::filter(region == select_region) %>%
      dplyr::mutate(value = round(value))
  }

  fig <-
    data %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = age,
        y = value,
        col = var_name
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_hue(c = 50, l = 50, h = c(30, 300)) +
    ggplot2::geom_smooth(
      method = "gam",
      se = FALSE,
      formula = y ~ s(x, bs = "cs"),
      method.args = list(
        family =
          stats::binomial(link = "logit")
      )
    ) +
    ggplot2::facet_wrap(~ecozone_koppen_15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = select_region, x = "")

  return(fig)
}
