#' @title Prepare joint human-proxy HVarPart inputs
#' @description
#' Join transformed SPD, KK10, and HYDE values to the canonical within-dataset
#' and region-by-age H1 input structures over their common age interval.
#' @param data_within_dataset Nested H1 data with `dataset_id` and `data_merge`.
#' @param data_time_slices Nested H1 data with `region`, `age`, and `data_merge`.
#' @param data_proxy_matches Fully matched proxy data from the convergence
#'   pipeline.
#' @param data_metadata Dataset metadata containing `dataset_id` and `region`.
#' @param age_min Youngest retained age in years BP.
#' @param age_max Oldest retained age in years BP.
#' @return A list with `within_dataset`, `time_slices`, and `matched_values`.
#' @examples
#' \dontrun{
#' prepare_joint_human_proxy_hvarpart_inputs(within, slices, proxies, metadata)
#' }
prepare_joint_human_proxy_hvarpart_inputs <- function(
  data_within_dataset,
  data_time_slices,
  data_proxy_matches,
  data_metadata,
  age_min = 2000,
  age_max = 8000
) {
  human_columns <-
    c("spd_transformed", "kk10_transformed", "hyde_transformed")
  proxy_columns <-
    c("dataset_id", "age_bp", "region", human_columns)

  assertthat::assert_that(
    is.data.frame(data_within_dataset),
    all(c("dataset_id", "data_merge") %in% names(data_within_dataset)),
    is.list(data_within_dataset[["data_merge"]]),
    is.data.frame(data_time_slices),
    all(c("region", "age", "data_merge") %in% names(data_time_slices)),
    is.list(data_time_slices[["data_merge"]]),
    is.data.frame(data_proxy_matches),
    all(proxy_columns %in% names(data_proxy_matches)),
    is.data.frame(data_metadata),
    all(c("dataset_id", "region") %in% names(data_metadata)),
    is.numeric(age_min),
    is.numeric(age_max),
    length(age_min) == 1L,
    length(age_max) == 1L,
    age_min < age_max,
    msg = "Joint human-proxy HVarPart inputs do not satisfy the contract."
  )

  assertthat::assert_that(
    !anyDuplicated(data_proxy_matches[c("dataset_id", "age_bp")]),
    !anyDuplicated(data_metadata["dataset_id"]),
    msg = "Proxy and metadata keys must be unique before HVarPart matching."
  )

  data_metadata_reduced <-
    data_metadata |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      metadata_region = as.character(.data[["region"]])
    )

  data_proxy <-
    data_proxy_matches |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      age = as.numeric(.data[["age_bp"]]),
      proxy_region = as.character(.data[["region"]]),
      spd_transformed = as.numeric(.data[["spd_transformed"]]),
      kk10_transformed = as.numeric(.data[["kk10_transformed"]]),
      hyde_transformed = as.numeric(.data[["hyde_transformed"]])
    ) |>
    dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
    dplyr::left_join(
      data_metadata_reduced,
      by = "dataset_id",
      relationship = "many-to-one"
    )

  assertthat::assert_that(
    nrow(data_proxy) > 0L,
    all(stats::complete.cases(data_proxy[human_columns])),
    all(purrr::map_lgl(data_proxy[human_columns], ~ all(is.finite(.x)))),
    all(!is.na(data_proxy[["metadata_region"]])),
    all(data_proxy[["proxy_region"]] == data_proxy[["metadata_region"]]),
    min(data_proxy[["age"]]) >= age_min,
    max(data_proxy[["age"]]) <= age_max,
    msg = paste(
      "Matched proxies must be finite, remain within the requested ages,",
      "and agree with canonical dataset regions."
    )
  )

  data_proxy_model <-
    data_proxy |>
    dplyr::select(
      dplyr::all_of(c("dataset_id", "age", human_columns))
    )

  data_within_prepared <-
    data_within_dataset |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      data_merge = purrr::map2(
        .data[["data_merge"]],
        .data[["dataset_id"]],
        ~ .x |>
          dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
          dplyr::mutate(.joint_dataset_id = .y) |>
          dplyr::inner_join(
            data_proxy_model,
            by = c(".joint_dataset_id" = "dataset_id", "age"),
            relationship = "many-to-one"
          ) |>
          dplyr::select(-dplyr::all_of(".joint_dataset_id")) |>
          dplyr::arrange(.data[["age"]])
      )
    ) |>
    dplyr::filter(purrr::map_int(.data[["data_merge"]], nrow) > 0L)

  data_slices_prepared <-
    data_time_slices |>
    dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
    dplyr::mutate(
      data_merge = purrr::map2(
        .data[["data_merge"]],
        .data[["age"]],
        ~ .x |>
          dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
          dplyr::inner_join(
            data_proxy_model |>
              dplyr::filter(.data[["age"]] == .y) |>
              dplyr::select(-dplyr::all_of("age")),
            by = "dataset_id",
            relationship = "many-to-one"
          )
      ),
      n_samples = purrr::map_int(.data[["data_merge"]], nrow)
    ) |>
    dplyr::filter(.data[["n_samples"]] > 0L)

  slice_regions <-
    data_slices_prepared |>
    dplyr::transmute(
      outer_region = .data[["region"]],
      age = .data[["age"]],
      data_merge = .data[["data_merge"]]
    ) |>
    tidyr::unnest(cols = dplyr::all_of("data_merge")) |>
    dplyr::select(
      dplyr::all_of(c("outer_region", "age", "dataset_id"))
    ) |>
    dplyr::left_join(
      data_proxy |>
        dplyr::select(
          dplyr::all_of(c("dataset_id", "age", "proxy_region"))
        ),
      by = c("dataset_id", "age"),
      relationship = "many-to-one"
    )

  assertthat::assert_that(
    all(slice_regions[["outer_region"]] == slice_regions[["proxy_region"]]),
    msg = "Time-slice regions must agree with matched proxy regions."
  )

  res <-
    list(
      within_dataset = data_within_prepared,
      time_slices = data_slices_prepared,
      matched_values = data_proxy
    )

  return(res)
}
