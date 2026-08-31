#' @title Plot temporal SPD-radius sensitivity
#' @description
#' Build separate region-row figures for selected-model human importance and
#' paired 500-minus-250 changes. The main profile uses zero-truncated,
#' renormalised shares; signed adjusted-R-squared fractions remain available
#' as a supplementary diagnostic. Both figures include the canonical blue
#' age-direction arrow.
#' @param data_all_available Long region-age radius source table.
#' @param data_paired Paired region-age comparison table.
#' @param metric_profile HVAR presentation profile.
#' @return A named list with `profiles` and `changes` ggplot objects.
#' @examples
#' \dontrun{
#' plot_spd_radius_temporal_comparison(all_available, paired)
#' }
plot_spd_radius_temporal_comparison <- function(
  data_all_available,
  data_paired,
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

  change_column <-
    stringr::str_c(profile_column, "_delta_500_minus_250")

  required_all <-
    c(
      "radius_km",
      "region",
      "age",
      "status",
      profile_column
    )

  required_paired <-
    c(
      "region",
      "age",
      change_column,
      "matched_estimable"
    )

  assertthat::assert_that(
    is.data.frame(data_all_available),
    all(required_all %in% names(data_all_available)),
    is.data.frame(data_paired),
    all(required_paired %in% names(data_paired)),
    msg = "Temporal SPD radius figure inputs do not satisfy the contract."
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")

  radius_region_labeller <- region_labeller

  radius_region_labeller[["Latin America"]] <-
    "Central &\nSouth America"

  data_values <-
    data_all_available |>
    dplyr::filter(
      .data[["status"]] %in%
        c("spatial_model_estimated", "no_spatial_terms_selected"),
      is.finite(.data[[profile_column]])
    ) |>
    dplyr::mutate(
      radius = factor(
        stringr::str_c(.data[["radius_km"]], " km"),
        levels = c("250 km", "500 km")
      ),
      region = factor(.data[["region"]], levels = region_levels)
    )

  profile_data_limits <-
    range(c(data_values[[profile_column]], 0), finite = TRUE)

  profile_padding <-
    max(diff(profile_data_limits) * 0.12, 0.01)

  profile_limits <-
    if (metric_profile == "zero_truncated") {
      c(0, 1)
    } else {
      c(
        profile_data_limits[[1]] - profile_padding,
        max(profile_data_limits[[2]] + profile_padding, 0.2)
      )
    }

  profile_background_values <-
    seq(profile_limits[[1]], profile_limits[[2]], length.out = 201)

  profile_background_step <-
    profile_background_values[[2]] - profile_background_values[[1]]

  data_profile_background <-
    tidyr::crossing(
      region = factor(region_levels, levels = region_levels),
      background_value = profile_background_values
    ) |>
    dplyr::mutate(
      ymin = .data[["background_value"]] - profile_background_step / 2,
      ymax = .data[["background_value"]] + profile_background_step / 2
    )

  displayed_ages <-
    sort(unique(data_values[["age"]]), decreasing = TRUE)

  age_step_ka <-
    min(diff(sort(displayed_ages))) / 1000

  age_limits_ka <-
    range(displayed_ages) / 1000

  arrow_inset_ka <-
    age_step_ka * 0.25

  profile_arrow_y <-
    profile_limits[[1]] - diff(profile_limits) * 0.18

  data_profile_arrow <-
    tibble::tibble(
      region = factor("Oceania", levels = region_levels),
      age = utils::head(displayed_ages, -2L),
      x = age / 1000,
      xend = x - age_step_ka,
      y = profile_arrow_y,
      yend = profile_arrow_y,
      arrow_colour = scales::col_numeric(
        palette = unname(paletete_age),
        domain = c(0, 8500)
      )(age)
    ) |>
    dplyr::mutate(
      x = pmin(
        .data[["x"]],
        age_limits_ka[[2]] - arrow_inset_ka
      ),
      xend = pmax(
        .data[["xend"]],
        age_limits_ka[[1]] + arrow_inset_ka
      )
    )

  data_profile_arrow_head <-
    tibble::tibble(
      region = factor("Oceania", levels = region_levels),
      x = age_limits_ka[[1]] + age_step_ka,
      xend = age_limits_ka[[1]] + arrow_inset_ka,
      y = profile_arrow_y,
      yend = profile_arrow_y
    )

  plot_profiles <-
    ggplot2::ggplot(
      data_values,
      ggplot2::aes(
        x = .data[["age"]] / 1000,
        y = .data[[profile_column]],
        group = .data[["radius"]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      switch = "y",
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(radius_region_labeller)
      )
    ) +
    ggplot2::scale_x_reverse(
      limits = rev(age_limits_ka),
      breaks = seq(8, 2, by = -2),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = scales::breaks_pretty(n = 4)(profile_limits),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_shape_manual(
      values = c("250 km" = 21, "500 km" = 22)
    ) +
    ggplot2::scale_linetype_manual(
      values = c("250 km" = "solid", "500 km" = "dashed")
    ) +
    ggplot2::scale_fill_gradient2(
      low = common_gray,
      mid = "#F2F2F2",
      high = palette_predictors[["human"]],
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1"),
      oob = scales::squish
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::labs(
      tag = "A",
      title = paste(
        "Sensitivity of temporal human importance",
        "to SPD search radius"
      ),
      x = "Age (cal ka BP)",
      y = if (metric_profile == "zero_truncated") {
        "Share of positive hierarchical contribution"
      } else {
        paste(
          "Untruncated signed human hierarchical contribution",
          "(adjusted-R-squared scale)",
          sep = "\n"
        )
      },
      shape = "SPD radius",
      linetype = "SPD radius",
      fill = "Human importance"
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_colourbar(
        order = 2,
        title.position = "left",
        direction = "horizontal",
        barwidth = grid::unit(28, "mm"),
        barheight = grid::unit(2, "mm")
      )
    ) +
    ggplot2::coord_cartesian(
      ylim = profile_limits,
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(size = text_size),
      plot.tag.position = c(0, 1),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        colour = common_gray
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.y = grid::unit(2, "mm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      plot.margin = ggplot2::margin(2, 2, 8, 2, unit = "mm")
    ) +
    ggplot2::geom_rect(
      data = data_profile_background,
      mapping = ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        fill = .data[["background_value"]]
      ),
      alpha = 0.2,
      colour = NA,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = common_gray,
      linetype = "dashed",
      linewidth = line_size
    ) +
    ggplot2::geom_line(
      ggplot2::aes(linetype = .data[["radius"]]),
      colour = common_gray,
      linewidth = line_size * 8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = .data[["radius"]],
        fill = .data[[profile_column]]
      ),
      colour = common_gray,
      size = point_size * 4,
      stroke = line_size * 3.5
    ) +
    ggplot2::geom_segment(
      data = data_profile_arrow,
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
      data = data_profile_arrow_head,
      mapping = ggplot2::aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
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

  data_delta <-
    data_paired |>
    dplyr::filter(
      .data[["matched_estimable"]],
      is.finite(.data[[change_column]])
    ) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels)
    )

  change_extent <-
    max(abs(data_delta[[change_column]]), 0.2, na.rm = TRUE)

  change_limits <-
    c(-change_extent, change_extent)

  change_background_values <-
    seq(change_limits[[1]], change_limits[[2]], length.out = 201)

  change_background_step <-
    change_background_values[[2]] - change_background_values[[1]]

  data_change_background <-
    tidyr::crossing(
      region = factor(region_levels, levels = region_levels),
      background_value = change_background_values
    ) |>
    dplyr::mutate(
      ymin = .data[["background_value"]] - change_background_step / 2,
      ymax = .data[["background_value"]] + change_background_step / 2
    )

  change_arrow_y <-
    change_limits[[1]] - diff(change_limits) * 0.32

  data_change_arrow <-
    data_profile_arrow |>
    dplyr::mutate(
      y = change_arrow_y,
      yend = change_arrow_y
    )

  data_change_arrow_head <-
    data_profile_arrow_head |>
    dplyr::mutate(
      y = change_arrow_y,
      yend = change_arrow_y
    )

  plot_changes <-
    ggplot2::ggplot(
      data_delta,
      ggplot2::aes(
        x = .data[["age"]] / 1000,
        y = .data[[change_column]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      switch = "y",
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(radius_region_labeller)
      )
    ) +
    ggplot2::scale_x_reverse(
      limits = rev(age_limits_ka),
      breaks = seq(8, 2, by = -2),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = scales::breaks_pretty(n = 5)(change_limits),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_fill_gradient2(
      low = common_gray,
      mid = "#F2F2F2",
      high = palette_predictors[["human"]],
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1"),
      oob = scales::squish
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::labs(
      tag = "B",
      title = paste(
        "Change in temporal human importance",
        "between SPD search radii"
      ),
      x = "Age (cal ka BP)",
      y = if (metric_profile == "zero_truncated") {
        paste(
          "Change in zero-truncated human share",
          "(500 km - 250 km)",
          sep = "\n"
        )
      } else {
        paste(
          "Change in untruncated signed human hierarchical contribution",
          "(adjusted-R-squared scale; 500 km - 250 km)",
          sep = "\n"
        )
      },
      fill = "Change in human importance"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        order = 1,
        title.position = "left",
        direction = "horizontal",
        barwidth = grid::unit(28, "mm"),
        barheight = grid::unit(2, "mm")
      )
    ) +
    ggplot2::coord_cartesian(
      ylim = change_limits,
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(size = text_size),
      plot.tag.position = c(0, 1),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        colour = common_gray
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.y = grid::unit(2, "mm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      plot.margin = ggplot2::margin(2, 2, 8, 2, unit = "mm")
    ) +
    ggplot2::geom_rect(
      data = data_change_background,
      mapping = ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        fill = .data[["background_value"]]
      ),
      alpha = 0.2,
      colour = NA,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = common_gray,
      linetype = "dashed",
      linewidth = line_size
    ) +
    ggplot2::geom_line(
      colour = common_gray,
      linewidth = line_size * 8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        fill = .data[[change_column]]
      ),
      shape = 21,
      colour = common_gray,
      size = point_size * 4,
      stroke = line_size * 3.5
    ) +
    ggplot2::geom_segment(
      data = data_change_arrow,
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
      data = data_change_arrow_head,
      mapping = ggplot2::aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
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

  res <-
    list(
      profiles = plot_profiles,
      changes = plot_changes
    )

  return(res)
}
