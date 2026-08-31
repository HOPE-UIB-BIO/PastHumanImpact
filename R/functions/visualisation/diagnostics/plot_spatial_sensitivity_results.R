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
    all(c("region", "age", "model_profile", "predictor", "individual") %in%
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
    ) |>
    dplyr::mutate(
      climatezone = .data[["omitted_group"]]
    ) |>
    prepare_climatezone_factor() |>
    dplyr::mutate(
      sensitivity_label = dplyr::recode(
        .data[["sensitivity_type"]],
        leave_region_out = "Region omitted",
        leave_climatezone_out = "Climate zone omitted"
      ),
      omitted_group_label = dplyr::case_when(
        .data[["sensitivity_type"]] == "leave_region_out" ~
          dplyr::recode(
            .data[["omitted_group"]],
            !!!region_labeller
          ),
        TRUE ~ as.character(.data[["climatezone"]])
      ),
      sensitivity_label = factor(
        .data[["sensitivity_label"]],
        levels = c("Region omitted", "Climate zone omitted")
      ),
      omitted_group_label = factor(
        .data[["omitted_group_label"]],
        levels = rev(c(
          unname(region_labeller),
          data_climate_zones[["climatezone_label"]]
        ))
      ),
      profile_label = dplyr::recode(
        .data[["profile"]],
        signed = "Untruncated signed contribution",
        zero_truncated = "Zero-truncated importance balance"
      ),
      profile_label = factor(
        .data[["profile_label"]],
        levels = c(
          "Untruncated signed contribution",
          "Zero-truncated importance balance"
        )
      )
    )
  plot_leave_out <-
    data_leave_out |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["omitted_group_label"]],
        y = .data[["importance_balance"]],
        shape = .data[["profile_label"]]
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = common_gray,
      linewidth = line_size
    ) +
    ggplot2::geom_point(
      colour = common_gray,
      fill = "white",
      size = point_size * 2.5,
      stroke = line_size * 3
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["sensitivity_label"]]),
      scales = "free_y"
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Untruncated signed contribution" = 21,
        "Zero-truncated importance balance" = 22
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Omitted group",
      y = paste(
        "Human-climate relative importance",
        "(see calculation profile)",
        sep = "\n"
      ),
      shape = "Calculation profile"
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.background = ggplot2::element_blank()
    )

  data_temporal_balance <-
    data_temporal_components |>
    dplyr::filter(.data[["predictor"]] %in% c("human", "climate")) |>
    dplyr::select(
      dplyr::all_of(c(
        "region",
        "age",
        "model_profile",
        "predictor",
        "individual"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "individual"
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
