#' @title Plot paired spatially controlled Figure 3 stacks
#' @description
#' Plot paired SPD and event stacks for human, climate, and space within every
#' region and faceted 500-year time slice.
#' @param data_stack Prepared spatially controlled temporal analysis stack table.
#' @param space_colour Muted colour assigned to structural spatial variation.
#' @param event_human_colour Orange assigned to event-based human influence.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' plot_h1_temporal_controlled_composition(stack_values)
#' }
plot_h1_temporal_controlled_composition <- function(
  data_stack,
  space_colour = "#A79BB8",
  event_human_colour = "#DC702E"
) {
  required_columns <-
    c("analysis", "region", "age", "predictor", "allocation")
  assertthat::assert_that(
    is.data.frame(data_stack),
    all(required_columns %in% names(data_stack)),
    assertthat::is.string(space_colour),
    assertthat::is.string(event_human_colour),
    msg = "spatially controlled temporal analysis inputs do not satisfy the contract."
  )

  region_levels <-
    c(
      "North America",
      "Latin America",
      "Europe",
      "Asia",
      "Oceania"
    )
  region_labels <- region_labeller
  region_labels[["Latin America"]] <-
    "Central &\nSouth America"
  age_levels <- seq(8500, 0, by = -500)
  age_labels <-
    scales::number(
      age_levels / 1000,
      accuracy = 0.1
    )
  bar_width <- 0.72
  data_plot <-
    data_stack |>
    dplyr::filter(
      dplyr::between(.data[["age"]], 0, 8500),
      .data[["analysis"]] != "temporal_spd" |
        .data[["age"]] >= 2000,
      is.finite(.data[["allocation"]])
    ) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels),
      age_facet = factor(
        .data[["age"]],
        levels = age_levels,
        labels = age_labels
      ),
      proxy = dplyr::recode(
        .data[["analysis"]],
        temporal_spd = "SPD",
        temporal_events = "Events"
      ),
      proxy = factor(.data[["proxy"]], levels = c("SPD", "Events")),
      predictor = factor(
        .data[["predictor"]],
        levels = c("space", "climate", "human")
      ),
      fill_group = dplyr::case_when(
        .data[["predictor"]] == "human" &
          .data[["proxy"]] == "Events" ~ "human_events",
        .data[["predictor"]] == "human" ~ "human_spd",
        .data[["predictor"]] == "climate" ~ "climate",
        .default = "space"
      ),
      fill_group = factor(
        .data[["fill_group"]],
        levels = c(
          "space",
          "climate",
          "human_spd",
          "human_events"
        )
      ),
      x_position = dplyr::if_else(
        .data[["proxy"]] == "SPD",
        1,
        2
      )
    )
  data_sums <-
    data_plot |>
    dplyr::summarise(
      allocation_sum = sum(.data[["allocation"]]),
      .by = c(
        "analysis",
        "region",
        "age_facet",
        "proxy",
        "x_position"
      )
    )
  assertthat::assert_that(
    all(abs(data_sums[["allocation_sum"]] - 1) < 1e-10),
    msg = "Every eligible Figure 3 stack must sum exactly to one."
  )
  data_outlines <-
    data_sums |>
    dplyr::mutate(
      xmin = .data[["x_position"]] - bar_width / 2,
      xmax = .data[["x_position"]] + bar_width / 2,
      ymin = 0,
      ymax = 1
    )
  displayed_ages <-
    age_levels[age_levels %in% data_plot[["age"]]]
  data_age_arrow <-
    tibble::tibble(
      region = factor("Oceania", levels = region_levels),
      age = displayed_ages,
      age_facet = factor(
        .data[["age"]],
        levels = age_levels,
        labels = age_labels
      ),
      x = 0.5,
      xend = 2.5,
      y = -0.52,
      yend = -0.52,
      arrow_colour = scales::col_numeric(
        palette = unname(paletete_age),
        domain = c(0, 8500)
      )(.data[["age"]])
    )
  data_arrow_head <-
    data_age_arrow |>
    dplyr::filter(.data[["age"]] == min(displayed_ages))

  result <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["x_position"]],
        y = .data[["allocation"]],
        fill = .data[["fill_group"]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      cols = ggplot2::vars(.data[["age_facet"]]),
      switch = "both",
      drop = TRUE,
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(region_labels)
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, 2.5),
      breaks = NULL,
      expand = ggplot2::expansion(add = 0)
    ) +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = c(0.25, 0.5, 0.75),
      labels = scales::label_percent(),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        human_spd = palette_predictors[["human"]],
        human_events = event_human_colour,
        climate = palette_predictors[["climate"]],
        space = space_colour
      ),
      breaks = c(
        "human_spd",
        "human_events",
        "climate",
        "space"
      ),
      labels = c(
        "Human (SPD)",
        "Human (events)",
        "Climate",
        "Space"
      )
    ) +
    ggplot2::scale_linetype_manual(
      name = "Human-impact proxy",
      values = c(SPD = "solid", Events = "dotted"),
      drop = FALSE
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(
        order = 2,
        override.aes = list(
          fill = "white",
          colour = common_gray
        )
      )
    ) +
    ggplot2::labs(
      x = "Age (cal ka BP)",
      y = paste0(
        "Relative importance\n",
        "(Zero-truncated hierarchical contribution)"
      ),
      fill = "Driver"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 1.02),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "center",
      legend.box.margin = ggplot2::margin(
        t = 4,
        r = 0,
        b = 0,
        l = 0,
        unit = "mm"
      ),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.x.bottom = ggplot2::element_text(
        size = text_size * 0.75,
        colour = common_gray
      ),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.line.x.bottom = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      ),
      axis.line.y.right = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      ),
      panel.spacing.x = grid::unit(1, "mm"),
      panel.spacing.y = grid::unit(2, "mm"),
      plot.margin = ggplot2::margin(
        t = 2,
        r = 2,
        b = 8,
        l = 2,
        unit = "mm"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = c(0.25, 0.5, 0.75),
      colour = colorspace::lighten(common_gray, amount = 0.65),
      linewidth = line_size
    ) +
    ggplot2::geom_col(
      width = bar_width,
      colour = NA
    ) +
    ggplot2::geom_rect(
      data = data_outlines,
      mapping = ggplot2::aes(
        xmin = .data[["xmin"]],
        xmax = .data[["xmax"]],
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        linetype = .data[["proxy"]]
      ),
      fill = NA,
      colour = common_gray,
      linewidth = line_size * 2.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = data_age_arrow,
      mapping = ggplot2::aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
        y = .data[["y"]],
        yend = .data[["yend"]],
        colour = .data[["arrow_colour"]]
      ),
      linewidth = line_size * 20,
      lineend = "butt",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = data_arrow_head,
      mapping = ggplot2::aes(
        x = 0.5,
        xend = 2.5,
        y = .data[["y"]],
        yend = .data[["yend"]]
      ),
      colour = paletete_age[["young"]],
      linewidth = line_size * 20,
      arrow = grid::arrow(
        length = grid::unit(3, "mm"),
        type = "closed"
      ),
      inherit.aes = FALSE
    )

  return(result)
}
