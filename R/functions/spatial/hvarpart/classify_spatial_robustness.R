#' @title Classify spatial robustness of the HVarPart ranking
#' @description
#' Apply the predeclared robustness thresholds to overall spatially filtered,
#' thinning, and deterministic leave-out ranking checks.
#' @param data_sensitivity Sensitivity summaries from
#' `summarise_spatial_importance_sensitivity()`.
#' @param data_spatial_estimates Estimates from `fit_spatial_importance()`.
#' @return One row per profile with agreement rates and robustness class.
#' @examples
#' \dontrun{
#' classify_spatial_robustness(
#'   data_sensitivity = sensitivity,
#'   data_spatial_estimates = spatial_estimates
#' )
#' }
classify_spatial_robustness <- function(
  data_sensitivity,
  data_spatial_estimates
) {
  required_sensitivity <-
    c(
      "sensitivity_type",
      "aggregation_level",
      "profile",
      "distance_km",
      "ranking"
    )
  required_spatial <-
    c("aggregation_level", "profile", "ranking")
  assertthat::assert_that(
    is.data.frame(data_sensitivity),
    all(required_sensitivity %in% names(data_sensitivity)),
    is.data.frame(data_spatial_estimates),
    all(required_spatial %in% names(data_spatial_estimates)),
    msg = "Robustness inputs do not satisfy the required contract."
  )

  data_overall <-
    data_sensitivity |>
    dplyr::filter(.data[["aggregation_level"]] == "overall")
  vec_profiles <- unique(data_overall[["profile"]])

  list_results <-
    vec_profiles |>
    purrr::map(
      .f = ~ {
        profile_name <- .x
        unthinned_ranking <-
          data_overall |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["sensitivity_type"]] == "unthinned"
          ) |>
          dplyr::pull("ranking") |>
          dplyr::first()
        spatial_ranking <-
          data_spatial_estimates |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["aggregation_level"]] == "overall"
          ) |>
          dplyr::pull("ranking") |>
          dplyr::first()
        data_thinning <-
          data_overall |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["sensitivity_type"]] == "thinning"
          ) |>
          dplyr::group_by(.data[["distance_km"]]) |>
          dplyr::summarise(
            agreement = mean(.data[["ranking"]] == unthinned_ranking),
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
          data_overall |>
          dplyr::filter(
            .data[["profile"]] == profile_name,
            .data[["sensitivity_type"]] %in% c(
              "leave_region_out",
              "leave_climatezone_out"
            )
          )
        deterministic_agreement <-
          nrow(data_deterministic) > 0L &&
          all(data_deterministic[["ranking"]] == unthinned_ranking)
        spatial_agreement <-
          length(spatial_ranking) == 1L &&
          !is.na(spatial_ranking) &&
          spatial_ranking == unthinned_ranking

        robustness_class <- dplyr::case_when(
          !spatial_agreement ~ "spatially_sensitive",
          is.finite(min_thinning_agreement) &&
            min_thinning_agreement < 0.8 ~ "spatially_sensitive",
          is.finite(min_thinning_agreement) &&
            min_thinning_agreement >= 0.95 &&
            deterministic_agreement ~ "robust",
          .default = "mixed"
        )

        tibble::tibble(
          profile = profile_name,
          unthinned_ranking = unthinned_ranking,
          spatial_ranking = spatial_ranking,
          spatial_agreement = spatial_agreement,
          min_thinning_agreement = min_thinning_agreement,
          deterministic_agreement = deterministic_agreement,
          robustness_class = robustness_class
        )
      }
    )

  res_classification <- dplyr::bind_rows(list_results)

  return(res_classification)
}
