#' @title Plot HVarPart importance for one core
#' @description
#' Plot the original HVarPart percentages and report total adjusted R-squared
#' for one dataset.
#' @param data_importance Long-format HVarPart importance data for one or more
#' datasets.
#' @param dataset_id Character scalar identifying the dataset to plot.
#' @param predictor_palette Named character vector with human and climate
#' colours.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' plot_hvarpart_importance(
#'   data_importance = data_importance,
#'   dataset_id = "40579"
#' )
#' }
plot_hvarpart_importance <- function(
  data_importance,
  dataset_id,
  predictor_palette = palette_predictors
) {
  required_columns <-
    c(
      "dataset_id",
      "predictor",
      "individual_percent",
      "total_adjusted_r_squared"
    )

  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_columns %in% names(data_importance)),
    is.character(dataset_id),
    length(dataset_id) == 1L,
    !is.na(dataset_id),
    is.character(predictor_palette),
    all(c("human", "climate") %in% names(predictor_palette)),
    msg = "HVarPart plotting inputs must be valid."
  )

  data_selected <-
    data_importance %>%
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]])
    ) %>%
    dplyr::filter(.data[["dataset_id"]] == .env$dataset_id) %>%
    dplyr::mutate(
      predictor = factor(
        .data[["predictor"]],
        levels = c("human", "climate")
      )
    )

  assertthat::assert_that(
    nrow(data_selected) == 2L,
    setequal(as.character(data_selected[["predictor"]]),
      c("human", "climate")),
    dplyr::n_distinct(
      data_selected[["total_adjusted_r_squared"]]
    ) == 1L,
    msg = "Each core must have human and climate HVarPart results."
  )

  total_adjusted_r_squared <-
    unique(data_selected[["total_adjusted_r_squared"]])
  res_plot <-
    ggplot2::ggplot(
      data = data_selected,
      mapping = ggplot2::aes(
        x = predictor,
        y = individual_percent,
        fill = predictor
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(suffix = "%"),
      expand = ggplot2::expansion(mult = c(0.12, 0.12))
    ) +
    ggplot2::scale_fill_manual(
      values = predictor_palette,
      breaks = c("human", "climate"),
      labels = c("Human", "Climate"),
      drop = FALSE
    ) +
    ggplot2::scale_x_discrete(
      labels = c(
        human = "Human",
        climate = "Climate"
      )
    ) +
    ggplot2::labs(
      title = "HVarPart",
      subtitle = stringr::str_glue(
        "Total adjusted R\u00b2: ",
        "{scales::number(total_adjusted_r_squared, accuracy = 0.001)}"
      ),
      x = NULL,
      y = "Individual importance",
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 9),
      plot.subtitle = ggplot2::element_text(size = 7),
      axis.title = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 7),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "none"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "grey50",
      linewidth = 0.3,
      linetype = 2
    ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = stringr::str_glue(
          "{round(individual_percent, 1)}%"
        ),
        vjust = dplyr::if_else(
          individual_percent >= 0,
          -0.3,
          1.3
        )
      ),
      colour = common_gray,
      size = 2.5
    )

  return(res_plot)
}
