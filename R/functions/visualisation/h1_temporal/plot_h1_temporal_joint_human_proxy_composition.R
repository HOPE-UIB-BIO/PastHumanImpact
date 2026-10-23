#' @title Plot joint human-proxy spatially controlled composition
#' @description
#' Plot one human, climate, and space stack for each region and 500-year age
#' slice in the joint SPD, KK10, and HYDE HVarPart sensitivity analysis.
#' @param data_stack Prepared spatially controlled composition table.
#' @param age_min Youngest displayed age in years BP.
#' @param age_max Oldest displayed age in years BP.
#' @param space_colour Colour assigned to structural spatial variation.
#' @param human_label Legend label for the joint human predictor block.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' plot_h1_temporal_joint_human_proxy_composition(stack_values)
#' }
plot_h1_temporal_joint_human_proxy_composition <- function(
  data_stack,
  age_min = 2000,
  age_max = 8000,
  space_colour = "#A79BB8",
  human_label = "Human (SPD + KK10 + HYDE)"
) {
  required_columns <-
    c("analysis", "region", "age", "predictor", "allocation")
  assertthat::assert_that(
    is.data.frame(data_stack),
    all(required_columns %in% names(data_stack)),
    is.numeric(age_min),
    is.numeric(age_max),
    age_min < age_max,
    assertthat::is.string(space_colour),
    assertthat::is.string(human_label),
    msg = "Joint human-proxy temporal plot inputs do not satisfy the contract."
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
  region_labels[["Latin America"]] <- "Central &\nSouth America"
  age_levels <- seq(age_max, age_min, by = -500)
  age_labels <- scales::number(age_levels / 1000, accuracy = 0.1)
  bar_width <- 0.72

  data_plot <-
    data_stack |>
    dplyr::filter(
      dplyr::between(.data[["age"]], age_min, age_max),
      is.finite(.data[["allocation"]]),
      .data[["predictor"]] %in% c("human", "climate", "space")
    ) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels),
      age_facet = factor(
        .data[["age"]],
        levels = age_levels,
        labels = age_labels
      ),
      predictor = factor(
        .data[["predictor"]],
        levels = c("space", "climate", "human")
      ),
      x_position = 1
    )

  data_sums <-
    data_plot |>
    dplyr::summarise(
      allocation_sum = sum(.data[["allocation"]]),
      .by = c("analysis", "region", "age_facet", "x_position")
    )
  assertthat::assert_that(
    nrow(data_sums) > 0L,
    all(abs(data_sums[["allocation_sum"]] - 1) < 1e-10),
    msg = "Every eligible joint human-proxy stack must sum exactly to one."
  )

  data_outlines <-
    data_sums |>
    dplyr::mutate(
      xmin = .data[["x_position"]] - bar_width / 2,
      xmax = .data[["x_position"]] + bar_width / 2,
      ymin = 0,
      ymax = 1
    )
  displayed_ages <- age_levels[age_levels %in% data_stack[["age"]]]
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
      xend = 1.5,
      y = -0.52,
      yend = -0.52,
      arrow_colour = scales::col_numeric(
        palette = unname(paletete_age),
        domain = c(age_min, age_max)
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
        fill = .data[["predictor"]]
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
      limits = c(0.5, 1.5),
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
        human = palette_predictors[["human"]],
        climate = palette_predictors[["climate"]],
        space = space_colour
      ),
      breaks = c("human", "climate", "space"),
      labels = c(
        human_label,
        "Climate",
        "Space"
      )
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::labs(
      x = "Age (cal ka BP)",
      y = "Share of positive hierarchical contribution",
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
      plot.margin = ggplot2::margin(2, 2, 8, 2, unit = "mm")
    ) +
    ggplot2::geom_hline(
      yintercept = c(0.25, 0.5, 0.75),
      colour = colorspace::lighten(common_gray, amount = 0.65),
      linewidth = line_size
    ) +
    ggplot2::geom_col(width = bar_width, colour = NA) +
    ggplot2::geom_rect(
      data = data_outlines,
      mapping = ggplot2::aes(
        xmin = .data[["xmin"]],
        xmax = .data[["xmax"]],
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]]
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
        xend = 1.5,
        y = .data[["y"]],
        yend = .data[["yend"]]
      ),
      colour = paletete_age[["young"]],
      linewidth = line_size * 20,
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
      inherit.aes = FALSE
    )

  return(result)
}
