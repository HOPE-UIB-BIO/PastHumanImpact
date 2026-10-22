#' @title Prepare HYDE-only HVarPart proxy matches
#' @description Prepare cached HYDE population without requiring SPD or KK10
#' observations, retaining zero population and canonical region membership.
#' @param data_hyde Data frame with unique `dataset_id`, `age_bp`, and `value`
#'   columns. Values must be finite non-negative HYDE population sums.
#' @param data_metadata Canonical unique `dataset_id` and `region` metadata.
#' @return A data frame with `dataset_id`, `age_bp`, `hyde_transformed` (square-root
#'   population), and `region`, preserving all supplied HYDE rows.
#' @examples
#' \dontrun{
#' prepare_hyde_hvarpart_matches(data_hyde, data_metadata)
#' }
prepare_hyde_hvarpart_matches <- function(data_hyde, data_metadata) {
  assertthat::assert_that(
    is.data.frame(data_hyde),
    all(c("dataset_id", "age_bp", "value") %in% names(data_hyde)),
    is.data.frame(data_metadata),
    all(c("dataset_id", "region") %in% names(data_metadata)),
    is.numeric(data_hyde[["value"]]),
    is.numeric(data_hyde[["age_bp"]]),
    all(is.finite(data_hyde[["value"]])),
    all(data_hyde[["value"]] >= 0),
    all(is.finite(data_hyde[["age_bp"]])),
    !anyNA(data_hyde[["dataset_id"]]),
    !anyDuplicated(data_hyde[c("dataset_id", "age_bp")]),
    !anyDuplicated(data_metadata["dataset_id"]),
    msg = "HYDE matches require unique keys and finite non-negative values."
  )

  data_matches <-
    data_hyde |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      age_bp = .data[["age_bp"]],
      hyde_transformed = sqrt(.data[["value"]])
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
    cli::cli_abort("Every HYDE dataset must have canonical region metadata.")
  }

  return(data_matches)
}
