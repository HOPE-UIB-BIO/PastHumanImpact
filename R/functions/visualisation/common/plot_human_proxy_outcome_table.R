#' @title Plot the human-proxy outcome table
#' @description
#' Show the headline human-minus-climate balance and its 95 % interval for
#' every main human proxy and cell of the outcome table, coloured by outcome
#' class, with one panel per scale and window.
#' @param data_summary
#' Headline rows of the outcome table (`summary` of
#' `summarise_human_proxy_outcome_table()`) with columns `analysis`,
#' `human_proxy`, `gridded_support`, `age_window`, `record_set`, `estimate`,
#' `lwr_95`, `upr_95`, `outcome_class`, and
#' `class_changes_between_profiles`.
#' @param proxy_labels
#' Named character vector of displayed proxy names; its names select and
#' order the proxies (first on top).
#' @param analysis_labels
#' Named character vector of displayed scale names (panel rows).
#' @param window_labels
#' Named character vector of displayed window names (panel columns).
#' @return
#' A ggplot object. Points and interval lines are coloured by outcome class
#' (human: `palette_predictors[["human"]]`, climate:
#' `palette_predictors[["climate"]]`, no clear dominance: `common_gray`).
#' Point shape shows the support of the gridded proxies (circle: SPD buffer,
#' triangle: 50 km) and filling the record set (filled: records shared by the
#' three proxies, open: all records of the run). An asterisk marks cells
#' whose class changes between profiles.
#' @details
#' Rows of proxies not named in `proxy_labels` are dropped. Within each proxy
#' the four support and record-set variants are drawn at fixed vertical
#' offsets, so identical estimates never overlap.
#' @examples
#' \dontrun{
#' plot_human_proxy_outcome_table(outcome_summary)
#' }
plot_human_proxy_outcome_table <- function(
  data_summary,
  proxy_labels = c(
    spd = "SPD",
    kk10 = "KK10",
    hyde_3_5_land_use = "HYDE 3.5 land use"
  ),
  analysis_labels = c(
    spatial_h1 = "Spatial H1 (among records)",
    temporal_h1 = "Temporal H1 (through time)"
  ),
  window_labels = c(
    common_cohort = "Common cohort, 2.0–8.0 ka",
    native = "Native window of each proxy"
  )
) {
  vec_required <-
    c(
      "analysis", "human_proxy", "gridded_support", "age_window",
      "record_set", "estimate", "lwr_95", "upr_95", "outcome_class",
      "class_changes_between_profiles"
    )

  assertthat::assert_that(
    is.data.frame(data_summary),
    all(vec_required %in% names(data_summary)),
    any(data_summary[["human_proxy"]] %in% names(proxy_labels)),
    is.character(proxy_labels),
    !is.null(names(proxy_labels)),
    is.character(analysis_labels),
    is.character(window_labels),
    msg = "Outcome-table plot inputs do not satisfy the contract."
  )

  # Fixed offsets keep the four support and record-set variants of a proxy
  #    apart even when their estimates are identical.
  data_variants <-
    tibble::tibble(
      gridded_support = c(
        "spd_buffer", "spd_buffer", "radius_50km", "radius_50km"
      ),
      record_set = c(
        "shared_records", "all_records", "shared_records", "all_records"
      ),
      variant_label = c(
        "SPD buffer, shared records",
        "SPD buffer, all records",
        "50 km radius, shared records",
        "50 km radius, all records"
      ),
      variant_offset = c(0.27, 0.09, -0.09, -0.27),
      variant_shape = c(16, 1, 17, 2)
    )

  vec_class_colours <-
    c(
      human = palette_predictors[["human"]],
      no_clear_dominance = common_gray,
      climate = palette_predictors[["climate"]]
    )

  data_plot <-
    data_summary |>
    dplyr::filter(.data[["human_proxy"]] %in% names(proxy_labels)) |>
    dplyr::inner_join(
      data_variants,
      by = c("gridded_support", "record_set")
    ) |>
    dplyr::mutate(
      proxy_position = match(.data[["human_proxy"]], rev(names(proxy_labels))),
      y_position = .data[["proxy_position"]] + .data[["variant_offset"]],
      analysis_label = factor(
        unname(analysis_labels[.data[["analysis"]]]),
        levels = unname(analysis_labels)
      ),
      window_label = factor(
        unname(window_labels[.data[["age_window"]]]),
        levels = unname(window_labels)
      ),
      variant_label = factor(
        .data[["variant_label"]],
        levels = dplyr::pull(data_variants, "variant_label")
      ),
      outcome_class = factor(
        .data[["outcome_class"]],
        levels = names(vec_class_colours)
      )
    )

  data_flags <-
    data_plot |>
    dplyr::filter(.data[["class_changes_between_profiles"]] %in% TRUE)

  vec_shapes <-
    rlang::set_names(
      dplyr::pull(data_variants, "variant_shape"),
      dplyr::pull(data_variants, "variant_label")
    )

  res <-
    data_plot |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data[["estimate"]],
        y = .data[["y_position"]],
        colour = .data[["outcome_class"]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["analysis_label"]]),
      cols = ggplot2::vars(.data[["window_label"]])
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(proxy_labels),
      labels = rev(unname(proxy_labels)),
      limits = c(0.5, length(proxy_labels) + 0.5),
      expand = ggplot2::expansion(add = 0)
    ) +
    ggplot2::scale_colour_manual(
      values = vec_class_colours,
      breaks = names(vec_class_colours),
      labels = c("Human", "No clear dominance", "Climate"),
      drop = FALSE
    ) +
    ggplot2::scale_shape_manual(values = vec_shapes) +
    ggplot2::labs(
      x = stringr::str_c(
        "Human-minus-climate balance, zero-truncated, with 95 % interval\n",
        "(negative: climate dominates; positive: human dominates)"
      ),
      y = NULL,
      colour = "Outcome class",
      shape = "Support of KK10 and HYDE",
      caption = stringr::str_c(
        "Shared records: units estimable for all three proxies. ",
        "SPD is always at its buffer.\n",
        "* The class differs in the signed or exclude-negative profile."
      )
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = text_size * 0.8),
      legend.text = ggplot2::element_text(size = text_size * 0.75),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = text_size * 0.85),
      axis.title.x = ggplot2::element_text(size = text_size * 0.8),
      plot.caption = ggplot2::element_text(
        size = text_size * 0.7,
        colour = common_gray
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = colorspace::lighten(common_gray, amount = 0.8),
        linewidth = line_size
      ),
      panel.spacing = grid::unit(4, "mm")
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2, nrow = 2)
    ) +
    ggplot2::geom_hline(
      yintercept = seq_len(length(proxy_labels) - 1L) + 0.5,
      colour = colorspace::lighten(common_gray, amount = 0.6),
      linewidth = line_size
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = common_gray,
      linetype = "dashed",
      linewidth = line_size * 3
    ) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(
        xmin = .data[["lwr_95"]],
        xmax = .data[["upr_95"]]
      ),
      linewidth = line_size * 5
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(shape = .data[["variant_label"]]),
      size = point_size * 1.6,
      stroke = 0.6
    ) +
    ggplot2::geom_text(
      data = data_flags,
      mapping = ggplot2::aes(x = .data[["upr_95"]]),
      label = "*",
      hjust = -0.4,
      vjust = 0.75,
      size = text_size * 0.4,
      show.legend = FALSE
    )

  return(res)
}
