#'' @title Plot temporal HVarPart importance balance
#'' @description
#'' Displays zero-truncated human-minus-climate balances as lollipops through
#'' time for the SPD and event human-impact proxies.
#'' @param data_balance Temporal importance-balance data produced by
#'' `compute_hvarpart_importance_balance()`.
#'' @param background Panel background style. `"gradient"` applies the same
#'' climate-white-human scale used by the points; `"white"` is retained as an
#'' explicit fallback.
#'' @param background_alpha Opacity of the gradient background.
#'' @param point_outline_multiplier Multiplier applied to `line_size` for point
#'' outlines.
#'' @return A ggplot object.
#'' @examples
#'' \dontrun{
#'' plot_hvarpart_temporal_balance(temporal_balance)
#'' }
plot_hvarpart_temporal_balance <- function(
    data_balance,
    background = c("gradient", "white"),
    background_alpha = 0.25,
    point_outline_multiplier = 7) {
  background <- match.arg(background)
  required <- c("analysis", "region", "age", "importance_balance")
  assertthat::assert_that(
    is.data.frame(data_balance),
    all(required %in% names(data_balance)),
    all(is.finite(data_balance[["importance_balance"]])),
    all(abs(data_balance[["importance_balance"]]) <= 1 + 1e-10),
    length(background_alpha) == 1,
    is.finite(background_alpha),
    background_alpha >= 0,
    background_alpha <= 1,
    length(point_outline_multiplier) == 1,
    is.finite(point_outline_multiplier),
    point_outline_multiplier > 0,
    msg = "Temporal importance-balance columns are missing or invalid."
  )

  data_plot <-
    data_balance |>
    dplyr::filter(
      dplyr::between(.data[["age"]], 0, 8500),
      .data[["analysis"]] != "temporal_spd" | .data[["age"]] >= 2000
    ) |>
    dplyr::mutate(
      analysis_label = dplyr::recode(
        .data[["analysis"]],
        temporal_spd = "SPD",
        temporal_events = "Events"
      ),
      analysis_label = factor(
        .data[["analysis_label"]],
        levels = c("SPD", "Events")
      ),
      age_ka = .data[["age"]] / 1000,
      region = factor(
        .data[["region"]],
        levels = c(
          "North America", "Latin America", "Europe", "Asia", "Oceania"
        )
      )
    )

  draw_proxy_key <- function(data, params, size) {
    get_key_value <- function(name, default) {
      value <- data[[name]]
      if (
        is.null(value) || length(value) == 0L || is.na(value[[1]])
      ) {
        return(default)
      }

      return(value[[1]])
    }
    key_colour <- get_key_value("colour", common_gray)
    key_fill <- get_key_value("fill", "#F2F2F2")
    key_linetype <- get_key_value("linetype", "solid")
    key_linewidth <- get_key_value("linewidth", 0.8)
    key_shape <- get_key_value("shape", 21)
    key_size <- get_key_value("size", point_size * 2.4)
    result <-
      grid::grobTree(
        grid::segmentsGrob(
          x0 = 0.1,
          x1 = 0.9,
          y0 = 0.5,
          y1 = 0.5,
          gp = grid::gpar(
            col = key_colour,
            lwd = key_linewidth * ggplot2::.pt,
            lty = key_linetype
          )
        ),
        grid::pointsGrob(
          x = 0.5,
          y = 0.5,
          pch = key_shape,
          size = grid::unit(key_size * ggplot2::.pt, "points"),
          gp = grid::gpar(
            col = key_colour,
            fill = key_fill,
            lwd = line_size * 7 * ggplot2::.pt
          )
        )
      )

    return(result)
  }

  result <- ggplot2::ggplot(data_plot)

  if (identical(background, "gradient")) {
    background_values <- seq(-1.2, 1.2, length.out = 241)
    background_step <- background_values[[2]] - background_values[[1]]
    data_background <-
      expand.grid(
        region = levels(data_plot[["region"]]),
        background_balance = background_values,
        stringsAsFactors = FALSE
      ) |>
      dplyr::mutate(
        region = factor(
          .data[["region"]],
          levels = levels(data_plot[["region"]])
        ),
        ymin = .data[["background_balance"]] - background_step / 2,
        ymax = .data[["background_balance"]] + background_step / 2
      )

    result <-
      result +
      ggplot2::geom_rect(
        data = data_background,
        mapping = ggplot2::aes(
          xmin = -Inf,
          xmax = Inf,
          ymin = .data[["ymin"]],
          ymax = .data[["ymax"]],
          fill = .data[["background_balance"]]
        ),
        colour = NA,
        alpha = background_alpha,
        inherit.aes = FALSE
      )
  }

  result <-
    result +
    ggplot2::geom_hline(
      yintercept = c(-1, 1),
      colour = colorspace::lighten(common_gray, amount = 0.55),
      linewidth = line_size
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = colorspace::lighten(common_gray, amount = 0.35),
      linewidth = line_size * 4,
      linetype = 2
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["importance_balance"]],
        group = .data[["analysis_label"]],
        linetype = .data[["analysis_label"]]
      ),
      colour = colorspace::lighten(common_gray, amount = 0.35),
      linewidth = line_size,
      alpha = 0.7,
      key_glyph = draw_proxy_key
    ) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = .data[["age_ka"]],
        xend = .data[["age_ka"]],
        y = 0,
        yend = .data[["importance_balance"]]
      ),
      colour = colorspace::lighten(common_gray, amount = 0.25),
      linewidth = line_size * 2
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["importance_balance"]],
        fill = .data[["importance_balance"]],
        shape = .data[["analysis_label"]]
      ),
      colour = common_gray,
      stroke = line_size * point_outline_multiplier,
      size = point_size * 2.4
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      switch = "y",
      drop = FALSE,
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(region_labeller)
      )
    ) +
    ggplot2::scale_x_reverse(
      limits = c(0, 8.5),
      breaks = seq(0, 8.5, 0.5),
      expand = ggplot2::expansion(add = 0.5)
    ) +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = c(-1, 0, 1),
      labels = c("Climate impact", "Equal", "Human impact"),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_fill_gradient2(
      low = palette_predictors[["climate"]],
      mid = "#F2F2F2",
      high = palette_predictors[["human"]],
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      guide = "none"
    ) +
    ggplot2::scale_shape_manual(
      "Human-impact proxy",
      values = c(SPD = 21, Events = 24),
      drop = FALSE,
      guide = "none"
    ) +
    ggplot2::scale_linetype_manual(
      "Human-impact proxy",
      values = c(SPD = "solid", Events = "dotted"),
      drop = FALSE
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        title = "Human-impact proxy",
        title.position = "top",
        nrow = 1,
        override.aes = list(
          shape = c(21, 24),
          fill = "#F2F2F2",
          linetype = c("solid", "dotted"),
          linewidth = 1,
          size = point_size * 2.4
        )
      )
    ) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = "Relative importance balance\n(human impact \u2212 climate)"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(-1.2, 1.2),
      clip = "on"
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.spacing.y = grid::unit(3, "mm"),
      strip.placement = "outside",
      strip.clip = "off",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        colour = common_gray,
        size = text_size * 0.8
      ),
      axis.text = ggplot2::element_text(
        colour = common_gray,
        size = text_size
      ),
      axis.title = ggplot2::element_text(
        colour = common_gray,
        size = text_size
      ),
      line = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      )
    )

  return(result)
}
