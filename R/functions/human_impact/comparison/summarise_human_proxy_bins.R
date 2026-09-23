#' @title Summarise matched human proxies in SPD-defined quantile bins
#' @description
#' Create overall and regional bins from transformed SPD values without
#' separating ties at duplicate quantile cut points.
#' @param data_matched Matched proxy table from
#'   `prepare_human_proxy_matches()`.
#' @param bin_count Requested number of quantile bins.
#' @param include_regions Logical. Include region-specific summaries.
#' @return Long-form bin summary for SPD, KK10, and HYDE.
#' @examples
#' \dontrun{
#' summarise_human_proxy_bins(data_matched, bin_count = 10L)
#' }
summarise_human_proxy_bins <- function(
  data_matched,
  bin_count = 10L,
  include_regions = TRUE
) {
  required_columns <-
    c(
      "dataset_id",
      "age_bp",
      "region",
      "spd_transformed",
      "kk10_transformed",
      "hyde_transformed"
    )

  assertthat::assert_that(
    is.data.frame(data_matched),
    all(required_columns %in% names(data_matched)),
    nrow(data_matched) > 0L,
    is.numeric(bin_count),
    length(bin_count) == 1L,
    bin_count == as.integer(bin_count),
    bin_count >= 2L,
    is.logical(include_regions),
    length(include_regions) == 1L,
    msg = "Human-proxy bin-summary inputs are invalid."
  )

  data_overall <-
    data_matched |>
    dplyr::mutate(
      scope_type = "overall",
      scope = "Overall"
    )

  data_scoped <-
    if (
      isTRUE(include_regions)
    ) {
      data_regional <-
        data_matched |>
        dplyr::filter(!is.na(.data[["region"]])) |>
        dplyr::mutate(
          scope_type = "region",
          scope = as.character(.data[["region"]])
        )

      dplyr::bind_rows(data_overall, data_regional)
    } else {
      data_overall
    }

  res_bins <-
    data_scoped |>
    dplyr::group_by(.data[["scope_type"]], .data[["scope"]]) |>
    dplyr::group_modify(
      ~ {
        vec_probabilities <-
          seq(
            from = 1 / as.integer(bin_count),
            to = (as.integer(bin_count) - 1L) / as.integer(bin_count),
            length.out = as.integer(bin_count) - 1L
          )

        vec_thresholds <-
          stats::quantile(
            .x[["spd_transformed"]],
            probs = vec_probabilities,
            na.rm = TRUE,
            names = FALSE,
            type = 8
          ) |>
          unique()

        maximum_spd <-
          max(.x[["spd_transformed"]], na.rm = TRUE)

        vec_breaks <-
          c(
            -Inf,
            vec_thresholds[vec_thresholds < maximum_spd],
            Inf
          ) |>
          unique()

        data_binned <-
          .x |>
          dplyr::mutate(
            bin_id = as.integer(cut(
              .data[["spd_transformed"]],
              breaks = vec_breaks,
              include.lowest = TRUE,
              labels = FALSE
            ))
          )

        data_counts <-
          data_binned |>
          dplyr::group_by(.data[["bin_id"]]) |>
          dplyr::summarise(
            n_rows = dplyr::n(),
            n_datasets = dplyr::n_distinct(.data[["dataset_id"]]),
            n_ages = dplyr::n_distinct(.data[["age_bp"]]),
            .groups = "drop"
          )

        data_binned |>
          tidyr::pivot_longer(
            cols = dplyr::all_of(
              c(
                "spd_transformed",
                "kk10_transformed",
                "hyde_transformed"
              )
            ),
            names_to = "proxy",
            values_to = "value"
          ) |>
          dplyr::mutate(
            proxy = stringr::str_remove(.data[["proxy"]], "_transformed$")
          ) |>
          dplyr::group_by(.data[["bin_id"]], .data[["proxy"]]) |>
          dplyr::summarise(
            median = stats::median(.data[["value"]]),
            range_025 = stats::quantile(
              .data[["value"]],
              probs = 0.025,
              names = FALSE,
              type = 8
            ),
            range_975 = stats::quantile(
              .data[["value"]],
              probs = 0.975,
              names = FALSE,
              type = 8
            ),
            .groups = "drop"
          ) |>
          dplyr::left_join(
            data_counts,
            by = "bin_id",
            relationship = "many-to-one"
          ) |>
          dplyr::mutate(
            bin_count_requested = as.integer(bin_count),
            bin_count_observed = dplyr::n_distinct(.data[["bin_id"]])
          )
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      .data[["scope_type"]],
      .data[["scope"]],
      .data[["bin_id"]],
      .data[["proxy"]]
    )

  return(res_bins)
}
