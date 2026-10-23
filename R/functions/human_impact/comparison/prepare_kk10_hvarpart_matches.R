#' @title Prepare KK10-only HVarPart proxy matches
#' @description Prepare cached KK10 fractions without requiring SPD or HYDE
#' observations, retaining zero fractions and canonical region membership.
#' @param data_kk10 Data frame with unique `dataset_id`, `age_bp`, and `value`
#'   columns. Values must be finite KK10 land-cover fractions in [0, 1].
#' @param data_metadata Canonical unique `dataset_id` and `region` metadata.
#' @return A tibble with `dataset_id`, `age_bp`, `kk10_transformed` (identity
#'   fraction), and `region`, preserving all supplied KK10 rows.
#' @examples
#' \dontrun{
#' prepare_kk10_hvarpart_matches(data_kk10, data_metadata)
#' }
prepare_kk10_hvarpart_matches <- function(data_kk10, data_metadata) {
  assertthat::assert_that(
    is.data.frame(data_kk10),
    all(c("dataset_id", "age_bp", "value") %in% names(data_kk10)),
    is.data.frame(data_metadata),
    all(c("dataset_id", "region") %in% names(data_metadata)),
    is.numeric(data_kk10[["value"]]),
    is.numeric(data_kk10[["age_bp"]]),
    all(is.finite(data_kk10[["value"]])),
    all(data_kk10[["value"]] >= 0),
    all(data_kk10[["value"]] <= 1),
    all(is.finite(data_kk10[["age_bp"]])),
    !anyNA(data_kk10[["dataset_id"]]),
    !anyDuplicated(data_kk10[c("dataset_id", "age_bp")]),
    !anyDuplicated(data_metadata["dataset_id"]),
    msg = "KK10 matches require unique keys and finite fractions in [0, 1]."
  )

  data_matches <-
    data_kk10 |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      age_bp = .data[["age_bp"]],
      kk10_transformed = .data[["value"]]
    ) |>
    dplyr::left_join(
      data_metadata |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          region = as.character(.data[["region"]])
        ),
      by = "dataset_id",
      relationship = "many-to-one"
    )

  if (
    anyNA(data_matches[["region"]])
  ) {
    cli::cli_abort("Every KK10 dataset must have canonical region metadata.")
  }

  return(data_matches)
}
