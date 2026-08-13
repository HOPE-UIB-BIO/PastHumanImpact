#' @title Prepare matched original Figure 2 records
#' @description
#' Convert matched two-group fields carried by spatiotemporally controlled spatial analysis records to
#' the common spatial sensitivity schema.
#' @param data_records Time-controlled Figure 2 record table.
#' @return Eligible matched human_climate_only records in the common balance schema.
#' @examples
#' \dontrun{
#' prepare_human_climate_only_records(time_controlled_records)
#' }
prepare_human_climate_only_records <- function(data_records) {
  required_columns <-
    c(
      "model_id",
      "region",
      "climatezone",
      "human_climate_only_signed_balance",
      "human_climate_only_signed_weight",
      "human_climate_only_zero_balance",
      "human_climate_only_zero_weight"
    )
  assertthat::assert_that(
    is.data.frame(data_records),
    all(required_columns %in% names(data_records)),
    msg = "Original Figure 2 record inputs do not satisfy the contract."
  )

  res_records <-
    data_records |>
    dplyr::mutate(
      signed_balance = .data[["human_climate_only_signed_balance"]],
      signed_weight = .data[["human_climate_only_signed_weight"]],
      zero_balance = .data[["human_climate_only_zero_balance"]],
      zero_weight = .data[["human_climate_only_zero_weight"]]
    ) |>
    dplyr::filter(
      is.finite(.data[["signed_balance"]]),
      is.finite(.data[["signed_weight"]]),
      .data[["signed_weight"]] > 0,
      is.finite(.data[["zero_balance"]]),
      is.finite(.data[["zero_weight"]]),
      .data[["zero_weight"]] > 0
    )

  return(res_records)
}
