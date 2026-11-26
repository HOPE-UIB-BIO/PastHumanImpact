#' @title Summarise human-proxy coverage by proxy, support, region, and age
#' @description
#' Count records with and without a value, and native against interpolated
#' values, overall and in each continental region.
#' @param data_values
#' Data frame in the `data_human_proxy_values` contract.
#' @param data_meta
#' Data frame with `dataset_id` and `region`, one row per record.
#' @return
#' Tibble with the columns `proxy`, `support`, `region`, `age_bp`,
#' `n_datasets`, `n_with_value`, `n_missing`, `n_native_layer`, and
#' `n_interpolated`. `region` is `All regions` for the overall rows and
#' `Unassigned` for records without a region. `n_native_layer` and
#' `n_interpolated` count only records with a finite value, so they sum to
#' `n_with_value`, and `n_with_value + n_missing == n_datasets`.
#' @examples
#' \dontrun{
#' summarise_human_proxy_coverage(data_values, data_meta)
#' }
summarise_human_proxy_coverage <- function(
  data_values,
  data_meta
) {
  assertthat::assert_that(
    is.data.frame(data_values),
    all(
      c("dataset_id", "age_bp", "proxy", "support", "value",
        "is_native_layer") %in% names(data_values)
    ),
    is.data.frame(data_meta),
    all(c("dataset_id", "region") %in% names(data_meta)),
    !anyDuplicated(as.character(data_meta[["dataset_id"]])),
    msg = "Human-proxy coverage inputs do not satisfy the contract."
  )

  data_meta_regions <-
    data_meta |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      region = as.character(.data[["region"]])
    ) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "region")))

  data_values_regions <-
    data_values |>
    dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
    dplyr::left_join(
      data_meta_regions,
      by = dplyr::join_by("dataset_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      region = dplyr::coalesce(.data[["region"]], "Unassigned"),
      has_value = is.finite(.data[["value"]])
    )

  res_coverage <-
    dplyr::bind_rows(
      data_values_regions |>
        dplyr::mutate(region = "All regions"),
      data_values_regions
    ) |>
    dplyr::group_by(
      .data[["proxy"]],
      .data[["support"]],
      .data[["region"]],
      .data[["age_bp"]]
    ) |>
    dplyr::summarise(
      n_datasets = dplyr::n_distinct(.data[["dataset_id"]]),
      n_with_value = sum(.data[["has_value"]]),
      n_missing = sum(!.data[["has_value"]]),
      n_native_layer = sum(
        .data[["has_value"]] & .data[["is_native_layer"]]
      ),
      n_interpolated = sum(
        .data[["has_value"]] & !.data[["is_native_layer"]]
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data[["proxy"]],
      .data[["support"]],
      .data[["region"]] != "All regions",
      .data[["region"]],
      dplyr::desc(.data[["age_bp"]])
    )

  return(res_coverage)
}
