#' @title Prepare canonical HVarPart control components
#' @description
#' Combine baseline and structurally controlled HVarPart summary tables while
#' normalising the external `Individual` field to canonical `individual`.
#' @param data_result One fitted temporal or spatial HVarPart result.
#' @param controlled_result_name Name of the controlled HVarPart list element.
#' @param controlled_profile Stable profile name for the controlled model.
#' @param include_total Whether to attach total adjusted R-squared values.
#' @return A canonical component table for the baseline and controlled models.
#' @examples
#' \dontrun{
#' prepare_hvarpart_control_components(
#'   data_result = result,
#'   controlled_result_name = "temporal_hvarpart",
#'   controlled_profile = "human_climate_time"
#' )
#' }
prepare_hvarpart_control_components <- function(
  data_result,
  controlled_result_name,
  controlled_profile,
  include_total = FALSE
) {
  assertthat::assert_that(
    is.list(data_result),
    assertthat::is.string(controlled_result_name),
    assertthat::is.string(controlled_profile),
    assertthat::is.flag(include_total),
    msg = "HVarPart component extraction inputs are invalid."
  )

  data_baseline_fit <- data_result[["human_climate_only_hvarpart"]]
  data_controlled_fit <- data_result[[controlled_result_name]]

  data_baseline <-
    if (
      is.null(data_baseline_fit)
    ) {
      tibble::tibble()
    } else {
      data_baseline_fit[["summary_table"]] |>
        dplyr::rename(individual = "Individual") |>
        dplyr::mutate(model_profile = "human_climate")
    }

  data_controlled <-
    if (
      is.null(data_controlled_fit)
    ) {
      tibble::tibble()
    } else {
      data_controlled_fit[["summary_table"]] |>
        dplyr::rename(individual = "Individual") |>
        dplyr::mutate(model_profile = controlled_profile)
    }

  if (
    include_total && nrow(data_baseline) > 0L
  ) {
    data_baseline[["total_adjusted_r_squared"]] <-
      data_baseline_fit[["varhp_output"]][[
        "Total_explained_variation"
      ]]
  }

  if (
    include_total && nrow(data_controlled) > 0L
  ) {
    data_controlled[["total_adjusted_r_squared"]] <-
      data_controlled_fit[["varhp_output"]][[
        "Total_explained_variation"
      ]]
  }

  return(dplyr::bind_rows(data_baseline, data_controlled))
}
