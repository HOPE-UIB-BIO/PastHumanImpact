#' @title Prepare common H1 result records
#' @description
#' Convert hierarchical contribution output to the common H1 long schema.
#' @param data_components HVarPart component table containing `predictor` and
#'   canonical lowercase `individual` columns.
#' @param data_status Optional model-status table.
#' @param profile_id Stable analysis profile identifier.
#' @param model_specification Stable model specification label.
#' @param proxy Human-impact proxy label.
#' @param analytical_unit Analytical unit label.
#' @param selected_control_dimensions Structural control label.
#' @param input_hash Upstream input fingerprint.
#' @param profile_hash Analysis-profile hash.
#' @param configuration_hash Configuration hash.
#' @param presentation_column Optional bounded-presentation value column.
#' @return Common-schema H1 result tibble.
#' @examples
#' \dontrun{
#' records <- prepare_h1_result_records(
#'   data_components = components,
#'   profile_id = "time_slice_spd_spatial_control",
#'   model_specification = "human_climate_space",
#'   proxy = "spd",
#'   analytical_unit = "time_slice",
#'   selected_control_dimensions = "space",
#'   input_hash = "input",
#'   profile_hash = "profile",
#'   configuration_hash = "config"
#' )
#' }
prepare_h1_result_records <- function(
  data_components,
  data_status = NULL,
  profile_id,
  model_specification,
  proxy,
  analytical_unit,
  selected_control_dimensions,
  input_hash,
  profile_hash,
  configuration_hash,
  presentation_column = NULL
) {
  assertthat::assert_that(
    is.data.frame(data_components),
    all(c("predictor", "individual") %in% names(data_components)),
    is.null(data_status) || is.data.frame(data_status),
    is.character(profile_id),
    length(profile_id) == 1L,
    is.character(model_specification),
    length(model_specification) == 1L,
    is.null(presentation_column) ||
      presentation_column %in% names(data_components),
    msg = "Common H1 result inputs are invalid."
  )

  key_columns <-
    base::intersect(
      c("dataset_id", "region", "climatezone", "age"),
      names(data_components)
    )

  data_values <-
    data_components |>
    dplyr::mutate(
      untruncated_value = .data[["individual"]],
      zero_value = pmax(.data[["individual"]], 0)
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(key_columns))
    ) |>
    dplyr::mutate(
      zero_total = sum(.data[["zero_value"]], na.rm = TRUE),
      presentation_value = dplyr::if_else(
        .data[["zero_total"]] > 0,
        .data[["zero_value"]] / .data[["zero_total"]],
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  if (
    !is.null(presentation_column)
  ) {
    data_values[["presentation_value"]] <-
      data_values[[presentation_column]]
  }

  status_columns <-
    if (
      is.null(data_status)
    ) {
      character()
    } else {
      base::intersect(
        c(
          key_columns,
          "status",
          "exclusion_reason",
          "design_rank",
          "residual_df",
          "n_selected"
        ),
        names(data_status)
      )
    }

  if (
    length(status_columns) > length(key_columns)
  ) {
    data_values <-
      data_values |>
      dplyr::left_join(
        data_status |>
          dplyr::select(dplyr::all_of(status_columns)),
        by = key_columns
      )
  }

  required_optional_columns <-
    c(
      "dataset_id",
      "region",
      "climatezone",
      "age",
      "status",
      "exclusion_reason",
      "design_rank",
      "residual_df",
      "n_selected"
    )

  missing_columns <-
    setdiff(required_optional_columns, names(data_values))

  data_values[missing_columns] <- NA

  res_records <-
    data_values |>
    tidyr::unite(
      col = "aggregation_key",
      dplyr::all_of(key_columns),
      remove = FALSE,
      sep = "|",
      na.rm = TRUE
    ) |>
    dplyr::transmute(
      profile_id = profile_id,
      model_specification = model_specification,
      proxy = proxy,
      analytical_unit = analytical_unit,
      dataset_id = as.character(.data[["dataset_id"]]),
      region = as.character(.data[["region"]]),
      climatezone = as.character(.data[["climatezone"]]),
      age = as.numeric(.data[["age"]]),
      aggregation_key = .data[["aggregation_key"]],
      contribution_type =
        "untruncated_hierarchical_contributions",
      predictor_group = .data[["predictor"]],
      untruncated_value = .data[["untruncated_value"]],
      presentation_value = .data[["presentation_value"]],
      model_status = as.character(.data[["status"]]),
      exclusion_reason = as.character(.data[["exclusion_reason"]]),
      selected_control_dimensions = selected_control_dimensions,
      rank = dplyr::coalesce(
        as.integer(.data[["design_rank"]]),
        as.integer(.data[["n_selected"]])
      ),
      residual_df = as.integer(.data[["residual_df"]]),
      input_hash = input_hash,
      profile_hash = profile_hash,
      configuration_hash = configuration_hash
    )

  return(res_records)
}
