#' @title Plot temporal human-event trajectories
#' @description
#' Plot modelled event probabilities and credible intervals across region and
#' climate-zone strata. Constant-response strata can be supplied with equal
#' estimates and interval bounds.
#' @param data_predictions Event predictions with uncertainty bounds.
#' @param event_palette Named character vector of event colours.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' plot_event_temporal_trends(
#'   data_predictions = data_event_predictions
#' )
#' }
plot_event_temporal_trends <- function(
  data_predictions,
  event_palette = c(
    bi = "grey60",
    fi = "#c99000",
    ei = "#a17400",
    ec = "#7b5800",
    cc = "#573e00",
    fc = "#00c92b",
    es = "#c9009e",
    weak = "#9b541b",
    medium = "#5d261a",
    strong = "#1f0000"
  )
) {
  required_columns <-
    c(
      "region",
      "climatezone",
      "age",
      "variable",
      "estimate",
      "conf_low",
      "conf_high"
    )
  event_labels <-
    c(
      bi = "no impact",
      fi = "first impact",
      ei = "emerging impact",
      ec = "extensive clearance",
      cc = "complete clearance",
      fc = "first cultivation",
      es = "European settlement",
      weak = "weak impact",
      medium = "medium impact",
      strong = "strong impact"
    )

  assertthat::assert_that(
    is.data.frame(data_predictions),
    all(required_columns %in% names(data_predictions)),
    msg = "Event predictions are missing required columns."
  )
  assertthat::assert_that(
    is.character(event_palette),
    !is.null(names(event_palette)),
    all(data_predictions[["variable"]] %in% names(event_labels)),
    all(data_predictions[["variable"]] %in% names(event_palette)),
    msg = "Variables must be recognised event codes with configured colours."
  )

  data_plot <-
    data_predictions %>%
    dplyr::mutate(
      event_label = factor(
        event_labels[as.character(variable)],
        levels = unname(event_labels)
      )
    )
  event_label_palette <-
    event_palette[names(event_labels)] %>%
    rlang::set_names(unname(event_labels))

  res_plot <-
    ggplot2::ggplot(
      data = data_plot,
      mapping = ggplot2::aes(
        x = age / 1000,
        y = estimate,
        colour = event_label,
        fill = event_label,
        group = event_label
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region),
      cols = ggplot2::vars(climatezone),
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(15),
        climatezone = ggplot2::label_wrap_gen(12)
      )
    ) +
    ggplot2::scale_x_reverse(
      limits = c(8.5, 0),
      breaks = c(8, 6, 4, 2, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    ggplot2::scale_colour_manual(
      values = event_label_palette,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = event_label_palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Age (cal ka BP)",
      y = "Predicted probability",
      colour = NULL,
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      strip.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      axis.title = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      axis.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      legend.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      )
    ) +
    ggplot2::guides(
      colour = "none",
      fill = ggplot2::guide_legend(
        nrow = 2,
        override.aes = list(alpha = 0.45)
      )
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = 0,
        ymax = estimate
      ),
      colour = NA,
      alpha = 0.14
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = conf_low,
        ymax = conf_high
      ),
      colour = NA,
      alpha = 0.25
    ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_vline(
      xintercept = 2,
      linetype = "dotted",
      linewidth = 0.35,
      colour = common_gray
    )

  return(res_plot)
}
