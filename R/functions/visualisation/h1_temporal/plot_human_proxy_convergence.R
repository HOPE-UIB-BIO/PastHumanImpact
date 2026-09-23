#' @title Plot SPD convergence with KK10 and HYDE
#' @description
#' Plot overall or regional binned medians and empirical 95 percent ranges for
#' the focal SPD against external human-impact proxies.
#' @param data_bins Long-form human-proxy bin summary.
#' @param data_correlations Correlation table with Kendall estimates.
#' @param scope_type One of `overall` or `region`.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' plot_human_proxy_convergence(bins, correlations)
#' }
plot_human_proxy_convergence <- function(
  data_bins,
  data_correlations,
  scope_type = "overall"
) {
  assertthat::assert_that(
    is.data.frame(data_bins),
    is.data.frame(data_correlations),
    scope_type %in% c("overall", "region"),
    msg = "Human-proxy convergence plot inputs are invalid."
  )

  data_spd <-
    data_bins |>
    dplyr::filter(
      .data[["scope_type"]] == .env$scope_type,
      .data[["proxy"]] == "spd"
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("scope", "bin_id", "median", "range_025", "range_975")
      )
    ) |>
    dplyr::rename(
      spd_median = "median",
      spd_range_025 = "range_025",
      spd_range_975 = "range_975"
    )

  data_plot <-
    data_bins |>
    dplyr::filter(
      .data[["scope_type"]] == .env$scope_type,
      .data[["proxy"]] %in% c("kk10", "hyde")
    ) |>
    dplyr::inner_join(
      data_spd,
      by = c("scope", "bin_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      proxy_label = dplyr::recode(
        .data[["proxy"]],
        kk10 = "KK10 land-use fraction",
        hyde = "Square-root HYDE population"
      )
    )

  data_annotations <-
    data_correlations |>
    dplyr::filter(.data[["scope_type"]] == .env$scope_type) |>
    dplyr::mutate(
      proxy_label = dplyr::recode(
        .data[["comparison_proxy"]],
        kk10 = "KK10 land-use fraction",
        hyde = "Square-root HYDE population"
      ),
      annotation = dplyr::if_else(
        .data[["eligible"]],
        stringr::str_glue(
          "Kendall tau = {format(round(kendall_tau, 2), nsmall = 2)}; ",
          "bins = {matched_bins}"
        ),
        stringr::str_glue("Insufficient coverage; bins = {matched_bins}")
      ),
      annotation = if (.env$scope_type == "region") {
        stringr::str_glue("{scope}: {annotation}")
      } else {
        .data[["annotation"]]
      }
    ) |>
    dplyr::group_by(.data[["proxy_label"]]) |>
    dplyr::arrange(.data[["scope"]], .by_group = TRUE) |>
    dplyr::mutate(
      annotation_vjust = 1.2 + (dplyr::row_number() - 1) * 1.1
    ) |>
    dplyr::ungroup()

  res_plot <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["spd_median"]],
        y = .data[["median"]],
        colour = .data[["scope"]],
        shape = .data[["scope"]]
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["proxy_label"]]),
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = "Square-root archaeological SPD",
      y = "External human-impact proxy",
      colour = NULL,
      shape = NULL
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      legend.position = if (scope_type == "overall") "none" else "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data[["range_025"]],
        ymax = .data[["range_975"]]
      ),
      width = 0,
      linewidth = 0.3,
      alpha = 0.55
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data[["spd_range_025"]],
        xmax = .data[["spd_range_975"]]
      ),
      orientation = "y",
      width = 0,
      linewidth = 0.3,
      alpha = 0.55
    ) +
    ggplot2::geom_path(
      ggplot2::aes(group = .data[["scope"]]),
      linewidth = 0.35,
      alpha = 0.65
    ) +
    ggplot2::geom_point(size = point_size + 1) +
    ggplot2::geom_text(
      data = data_annotations,
      ggplot2::aes(
        x = -Inf,
        y = Inf,
        label = .data[["annotation"]],
        colour = .data[["scope"]],
        vjust = .data[["annotation_vjust"]]
      ),
      inherit.aes = FALSE,
      hjust = -0.05,
      size = 3,
      show.legend = FALSE
    )

  return(res_plot)
}
