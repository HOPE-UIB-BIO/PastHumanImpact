#' @title Derive reduced PAP set from grouped selections
#' @description
#' Aggregates group-level PAP selections and keeps predictors selected in at
#' least `min_selected_fraction` of eligible groups.
#' @param data_collinearity Output list from `get_pap_collinearity()`.
#' @param min_selected_fraction Numeric in (0,1].
#' @return Character vector of retained PAP variables.
#' @examples
#' \dontrun{
#' data_collinearity <-
#'   list(
#'     group_diagnostics = tibble::tibble(
#'       region = c("A", "B"),
#'       is_eligible = c(TRUE, TRUE)
#'     ),
#'     selection_table = tibble::tibble(
#'       region = c("A", "A", "B"),
#'       is_eligible = c(TRUE, TRUE, TRUE),
#'       predictor = c("n0", "n1", "n0")
#'     )
#'   )
#'
#' get_pap_reduced_vars(
#'   data_collinearity = data_collinearity,
#'   min_selected_fraction = 0.5
#' )
#' }
get_pap_reduced_vars <- function(data_collinearity,
                                 min_selected_fraction = 0.5) {
  assertthat::assert_that(
    is.list(data_collinearity),
    msg = "`data_collinearity` must be a list."
  )
  assertthat::assert_that(
    all(c("group_diagnostics", "selection_table") %in%
      names(data_collinearity)),
    msg = "`data_collinearity` must include group_diagnostics and selection_table."
  )
  assertthat::assert_that(
    is.numeric(min_selected_fraction),
    length(min_selected_fraction) == 1,
    min_selected_fraction > 0,
    min_selected_fraction <= 1,
    msg = "`min_selected_fraction` must be in (0, 1]."
  )

  data_group_diagnostics <-
    data_collinearity[["group_diagnostics"]]

  n_groups_eligible <-
    sum(data_group_diagnostics[["is_eligible"]], na.rm = TRUE)

  if (n_groups_eligible == 0L) {
    cli::cli_abort("No eligible groups available to derive reduced PAP set.")
  }

  data_selection <- data_collinearity[["selection_table"]]

  data_selection <-
    data_selection[
      data_selection[["is_eligible"]] &
        !is.na(data_selection[["predictor"]]), ,
      drop = FALSE
    ]

  if (nrow(data_selection) == 0L) {
    cli::cli_abort("No selected PAP variables found in eligible groups.")
  }

  reduced_summary <-
    as.data.frame(
      table(data_selection[["predictor"]]),
      stringsAsFactors = FALSE
    )

  names(reduced_summary) <- c("predictor", "n_groups_selected")

  reduced_summary[["fraction_selected"]] <-
    reduced_summary[["n_groups_selected"]] / n_groups_eligible

  reduced_summary <-
    reduced_summary[
      reduced_summary[["fraction_selected"]] >= min_selected_fraction, ,
      drop = FALSE
    ]

  reduced_summary <-
    reduced_summary[
      order(
        -reduced_summary[["fraction_selected"]],
        reduced_summary[["predictor"]]
      ), ,
      drop = FALSE
    ]

  if (nrow(reduced_summary) == 0L) {
    cli::cli_abort(
      "No PAP variables passed selection threshold; lower min_selected_fraction."
    )
  }

  res_output <- reduced_summary[["predictor"]]

  return(res_output)
}
