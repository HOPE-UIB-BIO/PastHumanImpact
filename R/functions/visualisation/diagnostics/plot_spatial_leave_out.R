#' @title Plot leave-one-group-out spatial sensitivity
#' @description
#' Show the pooled human-climate result after omitting one continental group
#' or one region at a time, for both calculation profiles.
#' @param data_sensitivity
#' Spatial sensitivity table with columns `sensitivity_type`,
#' `aggregation_level`, `profile`, `importance_balance`, and
#' `omitted_group`.
#' @return A `ggplot` object with one panel per omission type.
#' @details
#' Only overall rows of the `leave_region_out` and `leave_climatezone_out`
#' sensitivity types are shown. Point shapes distinguish the untruncated
#' signed contribution from the zero-truncated importance balance.
#' @examples
#' \dontrun{
#' plot_spatial_leave_out(
#'   data_sensitivity = data_sensitivity
#' )
#' }
plot_spatial_leave_out <- function(data_sensitivity) {
  assertthat::assert_that(
    is.data.frame(data_sensitivity),
    all(
      c(
        "sensitivity_type",
        "aggregation_level",
        "profile",
        "importance_balance",
        "omitted_group"
      ) %in%
        names(data_sensitivity)
    ),
    msg = "Spatial leave-out inputs do not satisfy the contract."
  )

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
        leave_region_out = "Continental group omitted",
        leave_climatezone_out = "Region omitted"
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
        levels = c("Continental group omitted", "Region omitted")
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

  res_plot <-
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
      y = stringr::str_c(
        "Human-climate relative importance",
        "\n",
        "(see calculation profile)"
      ),
      shape = "Calculation profile"
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.background = ggplot2::element_blank()
    )

  return(res_plot)
}
