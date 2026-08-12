#' @title Summarise all profiles for one spatial subset
#' @description
#' Apply the signed and zero-truncated importance summaries at overall,
#' regional, and region-by-climate-zone aggregation levels.
#' @param data_subset Core-level spatial importance records.
#' @param sensitivity_type Sensitivity scenario label.
#' @param omitted_group Optional omitted-group label.
#' @param distance_value Optional thinning distance.
#' @param repetition_value Optional thinning repetition.
#' @return A common-schema spatial sensitivity summary tibble.
#' @examples
#' \dontrun{
#' summarise_spatial_importance_subset(
#'   data_subset = records,
#'   sensitivity_type = "baseline"
#' )
#' }
summarise_spatial_importance_subset <- function(
  data_subset,
  sensitivity_type,
  omitted_group = NA_character_,
  distance_value = NA_real_,
  repetition_value = NA_integer_
) {
  assertthat::assert_that(
    is.data.frame(data_subset),
    assertthat::is.string(sensitivity_type),
    msg = "Spatial importance subset inputs do not satisfy the contract."
  )

  data_profiles <-
    tibble::tribble(
      ~profile_name, ~balance_col, ~weight_col,
      "signed", "signed_balance", "signed_weight",
      "zero_truncated", "zero_balance", "zero_weight"
    )
  data_levels <-
    tibble::tibble(
      level_name = c("overall", "region", "region_climatezone"),
      group_vars = list(
        character(),
        "region",
        c("region", "climatezone")
      )
    )
  data_combinations <-
    tidyr::crossing(data_profiles, data_levels)
  res_summary <-
    data_combinations |>
    purrr::pmap_dfr(
      .f = ~ summarise_spatial_importance_profile(
        data_subset = data_subset,
        profile_name = ..1,
        balance_col = ..2,
        weight_col = ..3,
        level_name = ..4,
        group_vars = ..5,
        sensitivity_type = sensitivity_type,
        omitted_group = omitted_group,
        distance_value = distance_value,
        repetition_value = repetition_value
      )
    )

  return(res_summary)
}
