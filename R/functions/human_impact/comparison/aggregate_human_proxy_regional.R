#' @title Aggregate human proxies to region x climate zone x age
#' @description
#' Average record-level human-proxy values within each continental region,
#' climate zone, age, proxy, and support, for the H2 group-level analyses.
#' @param data_values
#' Data frame in the `data_human_proxy_values` contract.
#' @param data_meta
#' Data frame with `dataset_id`, `region`, and `climatezone`, one row per
#' record.
#' @return
#' Tibble with the columns `region`, `climatezone`, `age_bp`, `proxy`,
#' `support`, `value`, and `n_datasets`. `value` is the mean of the finite
#' record values in the group and `n_datasets` the number of such records.
#' @details
#' Records missing from `data_meta` or with a missing `region` or
#' `climatezone` are excluded. Groups without any finite value are dropped.
#' Buffer radii differ between records for the `spd_buffer` support, so the
#' grouping uses `support` and not `radius_km`.
#' @examples
#' \dontrun{
#' aggregate_human_proxy_regional(data_values, data_meta)
#' }
aggregate_human_proxy_regional <- function(
  data_values,
  data_meta
) {
  assertthat::assert_that(
    is.data.frame(data_values),
    all(
      c("dataset_id", "age_bp", "proxy", "support", "value") %in%
        names(data_values)
    ),
    is.data.frame(data_meta),
    all(c("dataset_id", "region", "climatezone") %in% names(data_meta)),
    !anyDuplicated(as.character(data_meta[["dataset_id"]])),
    msg = "Regional human-proxy inputs do not satisfy the contract."
  )

  data_meta_groups <-
    data_meta |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      region = as.character(.data[["region"]]),
      climatezone = as.character(.data[["climatezone"]])
    ) |>
    dplyr::filter(
      !is.na(.data[["region"]]),
      !is.na(.data[["climatezone"]])
    ) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "region", "climatezone")))

  res_regional <-
    data_values |>
    dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
    dplyr::filter(is.finite(.data[["value"]])) |>
    dplyr::inner_join(
      data_meta_groups,
      by = dplyr::join_by("dataset_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::group_by(
      .data[["region"]],
      .data[["climatezone"]],
      .data[["age_bp"]],
      .data[["proxy"]],
      .data[["support"]]
    ) |>
    dplyr::summarise(
      value = mean(.data[["value"]]),
      n_datasets = dplyr::n_distinct(.data[["dataset_id"]]),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data[["proxy"]],
      .data[["support"]],
      .data[["region"]],
      .data[["climatezone"]],
      dplyr::desc(.data[["age_bp"]])
    )

  return(res_regional)
}
