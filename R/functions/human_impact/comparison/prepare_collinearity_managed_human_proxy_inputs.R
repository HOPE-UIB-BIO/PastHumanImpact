#' @title Prepare matched human-proxy inputs for managed HVarPart models
#' @description Join raw and transformed matched proxies to canonical nested
#' H1 inputs, enforce unique keys and region agreement, and verify that raw
#' matched SPD agrees with canonical SPD.
#' @param data_within_dataset Nested within-dataset H1 inputs.
#' @param data_time_slices Nested region-by-age H1 inputs.
#' @param data_proxy_matches Issue #342 matched proxy observations.
#' @param data_metadata Canonical dataset metadata.
#' @param age_min Youngest retained age in years BP.
#' @param age_max Oldest retained age in years BP.
#' @param tolerance Numerical tolerance for the canonical SPD agreement check;
#'   the default reflects the canonical input's three-decimal storage precision.
#' @return A list containing prepared nested inputs, matched values, and proxy
#'   transformation provenance.
#' @examples
#' \dontrun{
#' prepare_collinearity_managed_human_proxy_inputs(within, slices, proxies, meta)
#' }
prepare_collinearity_managed_human_proxy_inputs <- function(
  data_within_dataset,
  data_time_slices,
  data_proxy_matches,
  data_metadata,
  age_min = 2000,
  age_max = 8000,
  tolerance = 5e-4
) {
  required_proxy <- c("dataset_id", "age_bp", "region", "spd", "kk10", "hyde")
  assertthat::assert_that(
    is.data.frame(data_within_dataset),
    all(c("dataset_id", "data_merge") %in% names(data_within_dataset)),
    is.data.frame(data_time_slices),
    all(c("region", "age", "data_merge") %in% names(data_time_slices)),
    is.data.frame(data_proxy_matches),
    all(required_proxy %in% names(data_proxy_matches)),
    is.data.frame(data_metadata),
    all(c("dataset_id", "region") %in% names(data_metadata)),
    !anyDuplicated(data_proxy_matches[c("dataset_id", "age_bp")]),
    !anyDuplicated(data_metadata["dataset_id"]),
    age_min < age_max,
    msg = "Collinearity-managed human-proxy inputs do not satisfy the contract."
  )

  proxy <- data_proxy_matches |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      age = as.numeric(.data[["age_bp"]]),
      proxy_region = as.character(.data[["region"]]),
      spd_raw = as.numeric(.data[["spd"]]),
      spd_sqrt = sqrt(.data[["spd"]]),
      kk10_fraction = as.numeric(.data[["kk10"]]),
      hyde_raw = as.numeric(.data[["hyde"]]),
      hyde_sqrt = sqrt(.data[["hyde"]])
    ) |>
    dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
    dplyr::left_join(
      data_metadata |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          metadata_region = as.character(.data[["region"]])
        ),
      by = "dataset_id", relationship = "many-to-one"
    )
  proxy_columns <- c(
    "spd_raw", "spd_sqrt", "kk10_fraction", "hyde_raw", "hyde_sqrt"
  )
  assertthat::assert_that(
    nrow(proxy) > 0L,
    all(stats::complete.cases(proxy[proxy_columns])),
    all(purrr::map_lgl(proxy[proxy_columns], ~ all(is.finite(.x)))),
    all(proxy[["proxy_region"]] == proxy[["metadata_region"]]),
    min(proxy[["age"]]) >= age_min,
    max(proxy[["age"]]) <= age_max,
    msg = "Matched proxies must be finite, in range, and agree with metadata regions."
  )
  proxy_model <- proxy |>
    dplyr::select(dplyr::all_of(c("dataset_id", "age", proxy_columns)))

  within <- data_within_dataset |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      data_merge = purrr::map2(
        .data[["data_merge"]],
        .data[["dataset_id"]],
        .f = ~ prepare_matched_human_proxy_dataset(
          data_dataset = .x,
          dataset_id = .y,
          data_proxy_model = proxy_model,
          age_min = age_min,
          age_max = age_max,
          tolerance = tolerance
        )
      )
    ) |>
    dplyr::filter(purrr::map_int(.data[["data_merge"]], nrow) > 0L)
  slices <- data_time_slices |>
    dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
    dplyr::mutate(
      data_merge = purrr::map2(
        .data[["data_merge"]], .data[["age"]],
        .f = ~ {
          .x |>
            dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
            dplyr::inner_join(
              proxy_model |>
                dplyr::filter(.data[["age"]] == .y) |>
                dplyr::select(-dplyr::all_of("age")),
              by = "dataset_id", relationship = "many-to-one"
            )
        }
      ),
      n_samples = purrr::map_int(.data[["data_merge"]], nrow)
    ) |>
    dplyr::filter(.data[["n_samples"]] > 0L)
  slice_regions <- slices |>
    dplyr::transmute(outer_region = .data[["region"]], age = .data[["age"]], data_merge = .data[["data_merge"]]) |>
    tidyr::unnest(cols = dplyr::all_of("data_merge")) |>
    dplyr::select(dplyr::all_of(c("outer_region", "age", "dataset_id"))) |>
    dplyr::left_join(
      proxy |>
        dplyr::select(dplyr::all_of(c("dataset_id", "age", "proxy_region"))),
      by = c("dataset_id", "age"), relationship = "many-to-one"
    )
  assertthat::assert_that(
    all(slice_regions[["outer_region"]] == slice_regions[["proxy_region"]]),
    msg = "Region-age inputs disagree with matched proxy regions."
  )
  provenance <- tibble::tribble(
    ~variable, ~source_column, ~transformation,
    "spd_raw", "spd", "identity",
    "spd_sqrt", "spd", "square_root",
    "kk10_fraction", "kk10", "identity_fraction",
    "hyde_raw", "hyde", "identity",
    "hyde_sqrt", "hyde", "square_root"
  )
  res <- list(
    within_dataset = within,
    time_slices = slices,
    matched_values = proxy,
    provenance = provenance
  )

  return(res)
}
