#' Plot split temporal human-event sensitivity figures
#'
#' @param data_all_available Matched-period continent-age results.
#' @param data_event_extension Event-only results younger than 2 ka BP.
#' @param data_matched Three-way matched continent-age comparison table.
#' @param event_human_colour Orange used for event-only human results.
#' @param metric_profile HVAR presentation profile. The default
#'   `"zero_truncated"` shows renormalised shares of positive contributions.
#'   `"untruncated_signed"` is a supplementary adjusted-R-squared diagnostic.
#'
#' @return Two all-data plots named `profiles` and `changes`.
#'
#' @export
plot_human_event_temporal_split_comparison <- function(
  data_all_available,
  data_event_extension,
  data_matched,
  event_human_colour = "#DC702E",
  metric_profile = c("zero_truncated", "untruncated_signed")
) {
  metric_profile <-
    match.arg(metric_profile)

  profile_column <-
    if (metric_profile == "zero_truncated") {
      "zero_allocation_human"
    } else {
      "controlled_human"
    }

  change_columns <-
    if (metric_profile == "zero_truncated") {
      c(
        "zero_allocation_human__spd_events_minus_spd",
        "zero_allocation_human__events_minus_spd"
      )
    } else {
      c(
        "controlled_human__spd_events_minus_spd",
        "controlled_human__events_minus_spd"
      )
    }

  change_prefix <-
    stringr::str_c(profile_column, "__")

  required_profiles <- c(
    "cohort", "proxy_variant", "region", "age", "status",
    profile_column
  )
  required_matched <- c(
    "cohort", "region", "age", "three_way_estimable",
    change_columns
  )
  assertthat::assert_that(
    is.data.frame(data_all_available),
    all(required_profiles %in% names(data_all_available)),
    is.data.frame(data_event_extension),
    all(required_profiles %in% names(data_event_extension)),
    is.data.frame(data_matched),
    all(required_matched %in% names(data_matched)),
    msg = "Split human-event temporal inputs do not satisfy the contract."
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")
  region_labels <- region_labeller
  region_labels[["Latin America"]] <- "Central &\nSouth America"
  proxy_labels <- c(
    spd = "SPD",
    spd_events = "Combined",
    events = "Events only"
  )
  proxy_colours <- c(
    spd = palette_predictors[["human"]],
    spd_events = palette_predictors[["human"]],
    events = event_human_colour
  )
  proxy_linetypes <- c(
    spd = "solid",
    spd_events = "dashed",
    events = "dotted"
  )
  proxy_shapes <- c(spd = 21, spd_events = 24, events = 22)
  contrast_labels <- c(
    spd_events_minus_spd = "Combined minus SPD",
    events_minus_spd = "Events only minus SPD"
  )
  contrast_colours <- c(
    spd_events_minus_spd = palette_predictors[["human"]],
    events_minus_spd = event_human_colour
  )
  contrast_linetypes <- c(
    spd_events_minus_spd = "dashed",
    events_minus_spd = "dotted"
  )
  contrast_shapes <- c(spd_events_minus_spd = 24, events_minus_spd = 22)

  valid_statuses <- c(
    "spatial_model_estimated",
    "no_spatial_terms_selected"
  )
  data_profiles <-
    dplyr::bind_rows(
      data_all_available |>
        dplyr::filter(
          .data[["cohort"]] == "as_coded",
          .data[["age"]] >= 2000
        ),
      data_event_extension |>
        dplyr::filter(
          .data[["cohort"]] == "as_coded",
          .data[["proxy_variant"]] == "events",
          .data[["age"]] < 2000
        )
    ) |>
    dplyr::filter(
      .data[["status"]] %in% valid_statuses,
      is.finite(.data[[profile_column]])
    ) |>
    dplyr::distinct(
      .data[["cohort"]], .data[["proxy_variant"]],
      .data[["region"]], .data[["age"]],
      .keep_all = TRUE
    ) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels),
      proxy_variant = factor(
        .data[["proxy_variant"]],
        levels = names(proxy_labels)
      )
    )
  data_changes <-
    data_matched |>
    dplyr::filter(
      .data[["cohort"]] == "as_coded",
      .data[["three_way_estimable"]]
    ) |>
    dplyr::select(dplyr::all_of(c(
      "cohort", "region", "age", change_columns
    ))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(change_columns),
      names_to = "contrast",
      names_prefix = change_prefix,
      values_to = "change"
    ) |>
    dplyr::filter(is.finite(.data[["change"]])) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels),
      contrast = factor(
        .data[["contrast"]],
        levels = names(contrast_labels)
      )
    )

  build_time_arrow <- function(minimum_age_ka, arrow_y) {
    x_values <- seq(8.25, minimum_age_ka + 0.75, by = -0.5)
    tibble::tibble(
      region = factor("Oceania", levels = region_levels),
      age = x_values * 1000,
      x = x_values,
      xend = x_values - 0.5,
      y = arrow_y,
      yend = arrow_y,
      arrow_colour = scales::col_numeric(
        palette = unname(paletete_age),
        domain = c(0, 8500)
      )(age)
    )
  }
  build_arrow_head <- function(minimum_age_ka, arrow_y) {
    tibble::tibble(
      region = factor("Oceania", levels = region_levels),
      x = minimum_age_ka + 0.75,
      xend = minimum_age_ka + 0.1,
      y = arrow_y,
      yend = arrow_y
    )
  }

  build_profile_plot <- function() {
    data_plot <- data_profiles

    profile_limits <-
      if (metric_profile == "zero_truncated") {
        c(-0.18, 1)
      } else {
        c(-0.32, 1)
      }

    profile_breaks <-
      if (metric_profile == "zero_truncated") {
        c(0, 0.5, 1)
      } else {
        c(-0.2, 0, 0.5, 1)
      }

    profile_label <-
      if (metric_profile == "zero_truncated") {
        "Share of positive hierarchical contribution"
      } else {
        paste(
          "Untruncated signed human hierarchical contribution",
          "(adjusted-R-squared scale)",
          sep = "\n"
        )
      }

    # Keep the time arrow in a reserved band inside the bottom panel. The
    # clearance from the lower coordinate limit prevents its thick stroke
    # from obscuring the x axis or tick labels.
    arrow_y <-
      if (metric_profile == "zero_truncated") -0.12 else -0.26

    data_arrow <- build_time_arrow(0, arrow_y)

    data_arrow_head <- build_arrow_head(0, arrow_y)

    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["age"]] / 1000,
        y = .data[[profile_column]],
        colour = .data[["proxy_variant"]],
        fill = .data[["age"]],
        linetype = .data[["proxy_variant"]],
        shape = .data[["proxy_variant"]],
        group = .data[["proxy_variant"]]
      )
    ) +
      ggplot2::annotate(
        geom = "rect",
        xmin = 0,
        xmax = 2,
        ymin = -Inf,
        ymax = Inf,
        fill = common_gray,
        alpha = 0.06
      ) +
      ggplot2::geom_vline(
        xintercept = 2,
        colour = common_gray,
        linetype = "dotted",
        linewidth = line_size * 2
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = common_gray,
        linetype = "dashed",
        linewidth = line_size * 2
      ) +
      ggplot2::geom_line(linewidth = line_size * 8) +
      ggplot2::geom_point(
        size = point_size * 1.8,
        stroke = line_size * 4
      ) +
      ggplot2::geom_segment(
        data = data_arrow,
        mapping = ggplot2::aes(
          x = .data[["x"]], xend = .data[["xend"]],
          y = .data[["y"]], yend = .data[["yend"]]
        ),
        colour = data_arrow[["arrow_colour"]],
        linewidth = line_size * 20,
        lineend = "butt",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_segment(
        data = data_arrow_head,
        mapping = ggplot2::aes(
          x = .data[["x"]], xend = .data[["xend"]],
          y = .data[["y"]], yend = .data[["yend"]]
        ),
        colour = paletete_age[["young"]],
        linewidth = line_size * 20,
        arrow = grid::arrow(
          length = grid::unit(3, "mm"),
          type = "closed"
        ),
        inherit.aes = FALSE
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["region"]]),
        switch = "y",
        labeller = ggplot2::labeller(
          region = ggplot2::as_labeller(region_labels)
        )
      ) +
      ggplot2::scale_x_reverse(
        limits = c(8.5, 0),
        breaks = c(8, 6, 4, 2, 0),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_y_continuous(
        position = "right",
        breaks = profile_breaks
      ) +
      ggplot2::scale_colour_manual(
        values = proxy_colours,
        labels = proxy_labels
      ) +
      ggplot2::scale_linetype_manual(
        values = proxy_linetypes,
        labels = proxy_labels
      ) +
      ggplot2::scale_shape_manual(
        values = proxy_shapes,
        labels = proxy_labels
      ) +
      ggplot2::scale_fill_gradientn(
        colours = unname(paletete_age),
        limits = c(0, 8500),
        guide = "none"
      ) +
      ggplot2::coord_cartesian(
        ylim = profile_limits,
        clip = "on",
        expand = FALSE
      ) +
      ggplot2::labs(
        x = "Age (cal ka BP)",
        y = profile_label,
        colour = "Human-impact proxy",
        linetype = "Human-impact proxy",
        shape = "Human-impact proxy",
        title = "All data",
        subtitle = paste(
          "Events only extends into the younger interval where",
          "archaeological SPD is unavailable"
        )
      ) +
      ggplot2::theme_classic(base_size = text_size) +
      ggplot2::theme(
        strip.placement = "outside",
        strip.background = ggplot2::element_blank(),
        strip.text.y.left = ggplot2::element_text(
          angle = 90,
          colour = common_gray
        ),
        panel.spacing.y = grid::unit(2, "mm"),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(2, 3, 10, 3, unit = "mm")
      )
  }

  build_change_plot <- function() {
    data_plot <- data_changes

    change_label <-
      if (metric_profile == "zero_truncated") {
        "Change in zero-truncated human share"
      } else {
        paste(
          "Change in untruncated signed human hierarchical contribution",
          "(adjusted-R-squared scale)",
          sep = "\n"
        )
      }

    change_limits <- c(-0.32, 0.2)
    arrow_y <- -0.26

    data_arrow <- build_time_arrow(2, arrow_y)

    data_arrow_head <- build_arrow_head(2, arrow_y)

    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["age"]] / 1000,
        y = .data[["change"]],
        colour = .data[["contrast"]],
        fill = .data[["age"]],
        linetype = .data[["contrast"]],
        shape = .data[["contrast"]],
        group = .data[["contrast"]]
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = common_gray,
        linetype = "dashed",
        linewidth = line_size * 2
      ) +
      ggplot2::geom_line(linewidth = line_size * 8) +
      ggplot2::geom_point(
        size = point_size * 1.8,
        stroke = line_size * 4
      ) +
      ggplot2::geom_segment(
        data = data_arrow,
        mapping = ggplot2::aes(
          x = .data[["x"]], xend = .data[["xend"]],
          y = .data[["y"]], yend = .data[["yend"]]
        ),
        colour = data_arrow[["arrow_colour"]],
        linewidth = line_size * 20,
        lineend = "butt",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_segment(
        data = data_arrow_head,
        mapping = ggplot2::aes(
          x = .data[["x"]], xend = .data[["xend"]],
          y = .data[["y"]], yend = .data[["yend"]]
        ),
        colour = paletete_age[["young"]],
        linewidth = line_size * 20,
        arrow = grid::arrow(
          length = grid::unit(3, "mm"),
          type = "closed"
        ),
        inherit.aes = FALSE
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["region"]]),
        switch = "y",
        labeller = ggplot2::labeller(
          region = ggplot2::as_labeller(region_labels)
        )
      ) +
      ggplot2::scale_x_reverse(
        limits = c(8.5, 2),
        breaks = c(8, 6, 4, 2),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::scale_y_continuous(
        position = "right",
        breaks = c(-0.2, -0.1, 0, 0.1, 0.2)
      ) +
      ggplot2::scale_colour_manual(
        values = contrast_colours,
        labels = contrast_labels
      ) +
      ggplot2::scale_linetype_manual(
        values = contrast_linetypes,
        labels = contrast_labels
      ) +
      ggplot2::scale_shape_manual(
        values = contrast_shapes,
        labels = contrast_labels
      ) +
      ggplot2::scale_fill_gradientn(
        colours = unname(paletete_age),
        limits = c(2000, 8500),
        guide = "none"
      ) +
      ggplot2::coord_cartesian(
        ylim = change_limits,
        clip = "on",
        expand = FALSE
      ) +
      ggplot2::labs(
        x = "Age (cal ka BP)",
        y = change_label,
        colour = "Comparison with SPD",
        linetype = "Comparison with SPD",
        shape = "Comparison with SPD",
        title = "All data",
        subtitle = paste(
          "Values close to zero show that changing the human-impact proxy",
          "has little effect"
        )
      ) +
      ggplot2::theme_classic(base_size = text_size) +
      ggplot2::theme(
        strip.placement = "outside",
        strip.background = ggplot2::element_blank(),
        strip.text.y.left = ggplot2::element_text(
          angle = 90,
          colour = common_gray
        ),
        panel.spacing.y = grid::unit(2, "mm"),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(2, 3, 10, 3, unit = "mm")
      )
  }

  return(list(
    profiles = build_profile_plot(),
    changes = build_change_plot()
  ))
}
