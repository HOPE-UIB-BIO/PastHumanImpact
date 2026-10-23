#' @title Prepare matched SPD, KK10, and HYDE observations
#' @description
#' Join the focal SPD with spatially and temporally aligned external proxies,
#' attach geographic metadata, and add Gordon-style transformations.
#' @param data_spd SPD data with `dataset_id`, `age_bp`, `spd`, and
#'   `radius_km`.
#' @param data_kk10 KK10 data with `dataset_id`, `age_bp`, and `value`.
#' @param data_hyde HYDE data with `dataset_id`, `age_bp`, and `value`.
#' @param data_metadata Metadata with `dataset_id` and `region`.
#' @param age_min Youngest retained age in years BP.
#' @param age_max Oldest retained age in years BP.
#' @return One row per fully matched dataset and age.
#' @examples
#' \dontrun{
#' prepare_human_proxy_matches(spd, kk10, hyde, metadata)
#' }
prepare_human_proxy_matches <- function(
  data_spd,
  data_kk10,
  data_hyde,
  data_metadata,
  age_min = 2000,
  age_max = 8000
) {
  assertthat::assert_that(
    is.data.frame(data_spd),
    all(c("dataset_id", "age_bp", "spd", "radius_km") %in%
      names(data_spd)),
    is.data.frame(data_kk10),
    all(c("dataset_id", "age_bp", "value") %in% names(data_kk10)),
    is.data.frame(data_hyde),
    all(c("dataset_id", "age_bp", "value") %in% names(data_hyde)),
    is.data.frame(data_metadata),
    all(c("dataset_id", "region") %in% names(data_metadata)),
    is.numeric(age_min),
    is.numeric(age_max),
    age_min < age_max,
    msg = "Matched human-proxy inputs do not satisfy the contract."
  )

  for (
    data_key in list(data_spd, data_kk10, data_hyde)
  ) {
    assertthat::assert_that(
      !anyDuplicated(data_key[c("dataset_id", "age_bp")]),
      msg = "Each proxy must contain unique dataset-age keys."
    )
  }

  metadata_reduced <-
    data_metadata |>
    dplyr::select(
      dplyr::any_of(c("dataset_id", "region", "long", "lat"))
    ) |>
    dplyr::distinct(.data[["dataset_id"]], .keep_all = TRUE)

  kk10_reduced <-
    data_kk10 |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      age_bp = .data[["age_bp"]],
      kk10 = .data[["value"]]
    )

  hyde_reduced <-
    data_hyde |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      age_bp = .data[["age_bp"]],
      hyde = .data[["value"]]
    )

  res_matched <-
    data_spd |>
    dplyr::filter(
      .data[["age_bp"]] >= age_min,
      .data[["age_bp"]] <= age_max
    ) |>
    dplyr::inner_join(
      kk10_reduced,
      by = c("dataset_id", "age_bp"),
      relationship = "one-to-one",
      suffix = c("", "_kk10")
    ) |>
    dplyr::inner_join(
      hyde_reduced,
      by = c("dataset_id", "age_bp"),
      relationship = "one-to-one",
      suffix = c("", "_hyde")
    ) |>
    dplyr::inner_join(
      metadata_reduced,
      by = "dataset_id",
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      is.finite(.data[["spd"]]),
      is.finite(.data[["kk10"]]),
      is.finite(.data[["hyde"]]),
      .data[["spd"]] >= 0,
      .data[["kk10"]] >= 0,
      .data[["hyde"]] >= 0
    ) |>
    dplyr::mutate(
      spd_transformed = sqrt(.data[["spd"]]),
      kk10_transformed = .data[["kk10"]],
      hyde_transformed = sqrt(.data[["hyde"]])
    ) |>
    dplyr::arrange(.data[["dataset_id"]], dplyr::desc(.data[["age_bp"]]))

  return(res_matched)
}
