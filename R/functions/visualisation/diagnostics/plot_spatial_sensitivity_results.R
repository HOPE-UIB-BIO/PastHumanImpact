#' @title Plot spatial sensitivity results
#' @description
#' Build the supplementary Moran, thinning, leave-out, HVarPart comparison,
#' and pure spatial-fraction plots from exported sensitivity tables.
#' @param data_moran Figure 2 Moran diagnostics.
#' @param data_sensitivity Figure 2 thinning and leave-out summaries.
#' @param data_estimates Spatially filtered Figure 2 estimates.
#' @param data_temporal_components Region-by-age HVarPart component table.
#' @param data_unique_adjusted_r2 Region-by-age pure partial fractions.
#' @return A named list of five `ggplot` objects.
#' @examples
#' \dontrun{
#' plot_spatial_sensitivity_results(
#'   data_moran = moran,
#'   data_sensitivity = sensitivity,
#'   data_estimates = estimates,
#'   data_temporal_components = components,
#'   data_unique_adjusted_r2 = partial
#' )
#' }
plot_spatial_sensitivity_results <- function(
  data_moran,
  data_sensitivity,
  data_estimates,
  data_temporal_components,
  data_unique_adjusted_r2
) {
  assertthat::assert_that(
    is.data.frame(data_moran),
    all(c(
      "distance_km",
      "moran_i",
      "stage",
      "profile",
      "spatial_scope"
    ) %in%
      names(data_moran)),
    is.data.frame(data_sensitivity),
    all(c(
      "sensitivity_type",
      "aggregation_level",
      "profile",
      "importance_balance",
      "distance_km",
      "omitted_group"
    ) %in% names(data_sensitivity)),
    is.data.frame(data_estimates),
    all(c("aggregation_level", "profile", "adjusted_balance") %in%
      names(data_estimates)),
    is.data.frame(data_temporal_components),
    all(c("region", "age", "model_profile", "predictor", "Individual") %in%
      names(data_temporal_components)),
    is.data.frame(data_unique_adjusted_r2),
    all(c("region", "age", "fraction", "adjusted_r_squared") %in%
      names(data_unique_adjusted_r2)),
    msg = "Spatial sensitivity plot inputs do not satisfy the contract."
  )

  plot_moran <-
    data_moran |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["distance_km"]],
        y = .data[["moran_i"]],
        colour = .data[["profile"]],
        shape = .data[["stage"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(
      ~spatial_scope,
      scales = "free_x",
      labeller = ggplot2::label_both
    ) +
    ggplot2::labs(
      x = "Distance threshold (km)",
      y = "Moran's I",
      colour = "Profile",
      shape = "Stage"
    ) +
    ggplot2::theme_classic()

  data_thinning <-
    data_sensitivity |>
    dplyr::filter(
      .data[["sensitivity_type"]] == "thinning",
      .data[["aggregation_level"]] == "overall"
    )
  data_unthinned <-
    data_sensitivity |>
    dplyr::filter(
      .data[["sensitivity_type"]] == "unthinned",
      .data[["aggregation_level"]] == "overall"
    )
  data_adjusted <-
    data_estimates |>
    dplyr::filter(.data[["aggregation_level"]] == "overall")
  plot_thinning <-
    data_thinning |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["importance_balance"]],
        fill = factor(.data[["distance_km"]])
      )
    ) +
    ggplot2::geom_histogram(
      bins = 25,
      alpha = 0.55,
      position = "identity"
    ) +
    ggplot2::geom_vline(
      data = data_unthinned,
      ggplot2::aes(xintercept = .data[["importance_balance"]]),
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = data_adjusted,
      ggplot2::aes(xintercept = .data[["adjusted_balance"]]),
      linetype = "solid",
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~profile, scales = "free_y") +
    ggplot2::labs(
      x = "Human minus climate importance",
      y = "Thinning repetitions",
      fill = "Distance (km)"
    ) +
    ggplot2::theme_classic()

  data_leave_out <-
    data_sensitivity |>
    dplyr::filter(
      .data[["sensitivity_type"]] %in% c(
        "leave_region_out",
        "leave_climatezone_out"
      ),
      .data[["aggregation_level"]] == "overall"
    )
  plot_leave_out <-
    data_leave_out |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(
          .data[["omitted_group"]],
          .data[["importance_balance"]]
        ),
        y = .data[["importance_balance"]],
        colour = .data[["profile"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_point(size = 2) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~sensitivity_type, scales = "free_y") +
    ggplot2::labs(
      x = "Omitted group",
      y = "Human minus climate importance",
      colour = "Profile"
    ) +
    ggplot2::theme_classic()

  data_temporal_balance <-
    data_temporal_components |>
    dplyr::filter(.data[["predictor"]] %in% c("human", "climate")) |>
    dplyr::select(
      dplyr::all_of(c(
        "region",
        "age",
        "model_profile",
        "predictor",
        "Individual"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "Individual"
    ) |>
    dplyr::mutate(
      importance_difference = .data[["human"]] - .data[["climate"]]
    )
  plot_temporal <-
    data_temporal_balance |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["age"]],
        y = .data[["importance_difference"]],
        colour = .data[["model_profile"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_x_reverse() +
    ggplot2::facet_wrap(~region) +
    ggplot2::labs(
      x = "Age (cal BP)",
      y = "Human minus climate individual contribution",
      colour = "HVarPart profile"
    ) +
    ggplot2::theme_classic()

  plot_partial <-
    data_unique_adjusted_r2 |>
    dplyr::filter(
      .data[["fraction"]] %in% c(
        "pure_human",
        "pure_climate",
        "pure_space"
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["age"]],
        y = .data[["adjusted_r_squared"]],
        colour = .data[["fraction"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_x_reverse() +
    ggplot2::facet_wrap(~region) +
    ggplot2::labs(
      x = "Age (cal BP)",
      y = "Pure adjusted R-squared",
      colour = "Fraction"
    ) +
    ggplot2::theme_classic()

  res_plots <-
    list(
      human_climate_balance_moran = plot_moran,
      human_climate_balance_spatial_thinning = plot_thinning,
      human_climate_balance_leave_out = plot_leave_out,
      time_control_hierarchical_contributions = plot_temporal,
      time_control_unique_adjusted_r2 = plot_partial
    )

  return(res_plots)
}
