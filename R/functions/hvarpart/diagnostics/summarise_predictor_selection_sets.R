#' @title Summarise local retained-predictor sets by geography
#' @description
#' Count complete retained human and climate predictor combinations within a
#' requested geographic grouping.
#' @param selection_by_dataset Dataset-level selected-predictor strings with
#' geographic metadata.
#' @param grouping_variable Name of the geographic grouping column.
#' @return A data frame with counts and proportions of each retained set.
#' @examples
#' \dontrun{summarise_predictor_selection_sets(selection, "climatezone")}
summarise_predictor_selection_sets <- function(
  selection_by_dataset,
  grouping_variable
) {
  assertthat::assert_that(
    is.data.frame(selection_by_dataset),
    is.character(grouping_variable), length(grouping_variable) == 1L,
    grouping_variable %in% names(selection_by_dataset),
    all(c(
      "selected_predictors_human", "selected_predictors_climate"
    ) %in% names(selection_by_dataset)),
    msg = "Predictor-set summary inputs do not satisfy the contract."
  )
  res <- selection_by_dataset |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("selected_predictors_"),
      names_to = "group", values_to = "selected_predictors",
      names_prefix = "selected_predictors_"
    ) |>
    dplyr::mutate(
      selected_predictors = tidyr::replace_na(
        .data[["selected_predictors"]], "None"
      )
    ) |>
    dplyr::summarise(
      n_datasets = dplyr::n(),
      .by = dplyr::all_of(c(
        grouping_variable, "group", "selected_predictors"
      ))
    ) |>
    dplyr::mutate(
      proportion = .data[["n_datasets"]] / sum(.data[["n_datasets"]]),
      .by = dplyr::all_of(c(grouping_variable, "group"))
    ) |>
    dplyr::arrange(
      .data[[grouping_variable]], .data[["group"]],
      dplyr::desc(.data[["n_datasets"]])
    )
  return(res)
}
