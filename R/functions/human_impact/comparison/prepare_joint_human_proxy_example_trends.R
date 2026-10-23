#' @title Prepare joint human-proxy trends for dataset examples
#' @description
#' Convert the matched SPD, KK10, and HYDE product to the long plotting contract
#' used by dataset-level temporal figures. Values are restricted to observed
#' matched ages; the function never extrapolates proxy trajectories.
#' @param data_proxy_matches Matched human-proxy data containing one row per
#' dataset and age.
#' @param data_metadata Dataset metadata containing region and climate zone.
#' @param dataset_ids Character vector of datasets to retain.
#' @param variables Human proxies to include. Supported values are `spd`,
#' `kk10`, and `hyde`.
#' @param age_min,age_max Inclusive age limits in calibrated years BP.
#' @return A long tibble compatible with the observed-data input of
#' `plot_dataset_temporal_trends()`.
#' @examples
#' \dontrun{
#' prepare_joint_human_proxy_example_trends(
#'   data_proxy_matches = data_human_proxy_matches,
#'   data_metadata = data_meta,
#'   dataset_ids = c("14944", "15394"),
#'   variables = c("kk10", "hyde")
#' )
#' }
prepare_joint_human_proxy_example_trends <- function(
  data_proxy_matches,
  data_metadata,
  dataset_ids,
  variables = c("spd", "kk10", "hyde"),
  age_min = 2000,
  age_max = 8000
) {
  supported_variables <- c("spd", "kk10", "hyde")
  required_proxy_columns <-
    c("dataset_id", "age_bp", "region", supported_variables)
  required_metadata_columns <-
    c("dataset_id", "region", "climatezone")

  assertthat::assert_that(
    is.data.frame(data_proxy_matches),
    is.data.frame(data_metadata),
    all(required_proxy_columns %in% names(data_proxy_matches)),
    all(required_metadata_columns %in% names(data_metadata)),
    is.character(dataset_ids),
    length(dataset_ids) > 0L,
    !anyNA(dataset_ids),
    !anyDuplicated(dataset_ids),
    is.character(variables),
    length(variables) > 0L,
    !anyNA(variables),
    !anyDuplicated(variables),
    all(variables %in% supported_variables),
    is.numeric(age_min),
    length(age_min) == 1L,
    is.numeric(age_max),
    length(age_max) == 1L,
    is.finite(age_min),
    is.finite(age_max),
    age_min <= age_max,
    msg = "Joint human-proxy example inputs do not satisfy the contract."
  )

  data_metadata_selected <-
    data_metadata |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      metadata_region = as.character(.data[["region"]]),
      climatezone = .data[["climatezone"]]
    ) |>
    dplyr::filter(.data[["dataset_id"]] %in% dataset_ids) |>
    dplyr::distinct()

  assertthat::assert_that(
    nrow(data_metadata_selected) == length(dataset_ids),
    !anyDuplicated(data_metadata_selected[["dataset_id"]]),
    msg = "Each selected dataset requires one metadata region and climate zone."
  )

  data_selected <-
    data_proxy_matches |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      age = as.numeric(.data[["age_bp"]]),
      proxy_region = as.character(.data[["region"]]),
      spd = as.numeric(.data[["spd"]]),
      kk10 = as.numeric(.data[["kk10"]]),
      hyde = as.numeric(.data[["hyde"]])
    ) |>
    dplyr::filter(
      .data[["dataset_id"]] %in% dataset_ids,
      dplyr::between(.data[["age"]], age_min, age_max)
    )

  assertthat::assert_that(
    nrow(data_selected) > 0L,
    !anyDuplicated(data_selected[c("dataset_id", "age")]),
    setequal(unique(data_selected[["dataset_id"]]), dataset_ids),
    all(vapply(
      data_selected[variables],
      function(x) all(is.finite(x)),
      logical(1)
    )),
    msg = paste(
      "Selected proxy histories must have unique dataset-age keys,",
      "complete dataset coverage, and finite requested values."
    )
  )

  res_trends <-
    data_selected |>
    dplyr::left_join(data_metadata_selected, by = "dataset_id") |>
    dplyr::mutate(
      region_agrees =
        .data[["proxy_region"]] == .data[["metadata_region"]]
    )

  assertthat::assert_that(
    all(res_trends[["region_agrees"]]),
    msg = "Proxy and metadata regions disagree for a selected dataset."
  )

  res_trends <-
    res_trends |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      variable = .data[["variable"]],
      variable_label = resolve_temporal_variable_label(.data[["variable"]]),
      region = .data[["metadata_region"]],
      climatezone = .data[["climatezone"]],
      age = .data[["age"]],
      value = .data[["value"]]
    ) |>
    dplyr::arrange(.data[["dataset_id"]], .data[["variable"]], .data[["age"]])

  return(res_trends)
}
