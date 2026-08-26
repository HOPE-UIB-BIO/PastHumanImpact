#' @title Select one spatial balance estimate profile
#' @description
#' Select spatially adjusted balance estimates for one aggregation level and
#' one calculation profile while enforcing one estimate per plotted group.
#' @param data_estimates Data frame containing spatial balance estimates.
#' @param aggregation_level Character scalar selecting `"region"` or
#'   `"region_climatezone"` estimates.
#' @param profile Character scalar selecting `"zero_truncated"` or `"signed"`.
#' @return A data frame containing the selected, finite estimate rows.
#' @examples
#' \dontrun{
#' data_selected <- select_spatial_balance_estimates(
#'   data_estimates = data_estimates,
#'   aggregation_level = "region_climatezone",
#'   profile = "zero_truncated"
#' )
#' }
select_spatial_balance_estimates <- function(
  data_estimates,
  aggregation_level = c("region", "region_climatezone"),
  profile = c("zero_truncated", "signed")
) {
  required_columns <-
    c(
      "aggregation_level",
      "profile",
      "region",
      "climatezone",
      "adjusted_balance"
    )

  assertthat::assert_that(
    is.data.frame(data_estimates),
    all(required_columns %in% names(data_estimates)),
    msg = "`data_estimates` does not satisfy the balance estimate contract."
  )

  aggregation_level <-
    match.arg(aggregation_level)

  profile <-
    match.arg(profile)

  data_selected <-
    data_estimates |>
    dplyr::filter(
      .data[["aggregation_level"]] == .env[["aggregation_level"]],
      .data[["profile"]] == .env[["profile"]],
      is.finite(.data[["adjusted_balance"]])
    )

  if (
    nrow(data_selected) == 0L
  ) {
    cli::cli_abort(
      "No finite spatial balance estimates match the selected profile."
    )
  }

  key_columns <-
    if (
      identical(aggregation_level, "region_climatezone")
    ) {
      c("region", "climatezone")
    } else {
      "region"
    }

  data_duplicates <-
    data_selected |>
    dplyr::count(
      dplyr::across(dplyr::all_of(key_columns)),
      name = "n_estimates"
    ) |>
    dplyr::filter(.data[["n_estimates"]] > 1L)

  if (
    nrow(data_duplicates) > 0L
  ) {
    cli::cli_abort(
      "Spatial balance estimates must be unique for each plotted group."
    )
  }

  return(data_selected)
}
