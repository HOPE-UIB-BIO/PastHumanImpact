#' @title Classify spatiotemporally controlled spatial analysis robustness
#' @description
#' Compare the fully controlled, thinned, and deterministic sensitivity
#' directions with the matched original two-group Figure 2 direction.
#' @param data_sensitivity Controlled thinning and leave-out summaries.
#' @param data_spatial_estimates Fully controlled spatial-filter estimates.
#' @param data_human_climate_only Matched original two-group summaries.
#' @return One row per importance profile with prospective classification.
#' @examples
#' \dontrun{
#' classify_spatiotemporal_robustness(sensitivity, filtered, unthinned)
#' }
classify_spatiotemporal_robustness <- function(
  data_sensitivity,
  data_spatial_estimates,
  data_human_climate_only
) {
  required_summary <-
    c("sensitivity_type", "aggregation_level", "profile", "ranking")
  required_spatial <- c("aggregation_level", "profile", "ranking")
  assertthat::assert_that(
    is.data.frame(data_sensitivity),
    all(required_summary %in% names(data_sensitivity)),
    is.data.frame(data_spatial_estimates),
    all(required_spatial %in% names(data_spatial_estimates)),
    is.data.frame(data_human_climate_only),
    all(required_summary %in% names(data_human_climate_only)),
    msg = "Spatiotemporal robustness inputs do not satisfy the contract."
  )

  data_controlled <-
    data_sensitivity |>
    dplyr::filter(.data[["aggregation_level"]] == "overall")
  data_original <-
    data_human_climate_only |>
    dplyr::filter(
      .data[["aggregation_level"]] == "overall",
      .data[["sensitivity_type"]] == "human_climate_only"
    )
  vec_profiles <- unique(data_original[["profile"]])
  list_results <-
    vec_profiles |>
    purrr::map(
      .f = ~ {
        profile_name <- .x
        original_ranking <-
          data_original |>
          dplyr::filter(.data[["profile"]] == profile_name) |>
          dplyr::pull("ranking") |>
          dplyr::first()
        time_controlled_ranking <-
          data_controlled |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["sensitivity_type"]] == "unthinned"
          ) |>
          dplyr::pull("ranking") |>
          dplyr::first()
        fully_controlled_ranking <-
          data_spatial_estimates |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["aggregation_level"]] == "overall"
          ) |>
          dplyr::pull("ranking") |>
          dplyr::first()
        data_thinning <-
          data_controlled |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["sensitivity_type"]] == "thinning"
          ) |>
          dplyr::group_by(.data[["distance_km"]]) |>
          dplyr::summarise(
            agreement = mean(.data[["ranking"]] == original_ranking),
            .groups = "drop"
          )
        min_thinning_agreement <-
          if (
            nrow(data_thinning) == 0L
          ) {
            NA_real_
          } else {
            min(data_thinning[["agreement"]])
          }
        data_deterministic <-
          data_controlled |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["sensitivity_type"]] %in% c(
              "leave_region_out",
              "leave_climatezone_out"
            )
          )
        deterministic_agreement <-
          nrow(data_deterministic) > 0L &&
          all(data_deterministic[["ranking"]] == original_ranking)
        fully_controlled_agreement <-
          length(fully_controlled_ranking) == 1L &&
          !is.na(fully_controlled_ranking) &&
          fully_controlled_ranking == original_ranking
        robustness_class <- dplyr::case_when(
          !fully_controlled_agreement ~ "spatiotemporally_sensitive",
          is.finite(min_thinning_agreement) &&
            min_thinning_agreement < 0.8 ~
            "spatiotemporally_sensitive",
          is.finite(min_thinning_agreement) &&
            min_thinning_agreement >= 0.95 &&
            deterministic_agreement ~ "robust",
          .default = "mixed"
        )

        tibble::tibble(
          profile = profile_name,
          original_ranking = original_ranking,
          time_controlled_ranking = time_controlled_ranking,
          fully_controlled_ranking = fully_controlled_ranking,
          fully_controlled_agreement = fully_controlled_agreement,
          min_thinning_agreement = min_thinning_agreement,
          deterministic_agreement = deterministic_agreement,
          robustness_class = robustness_class
        )
      }
    )

  return(dplyr::bind_rows(list_results))
}
