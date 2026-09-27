#' @title Select predictors within one local HVarPart group
#' @description Remove invalid columns and apply response-independent
#' correlation and VIF filtering to one conceptual predictor group.
#' @param data_source Data frame for one analytical unit.
#' @param candidates Candidate predictor names.
#' @param preference Deterministic predictor preference order.
#' @param group_name Group identifier used in the audit table.
#' @param max_cor Maximum absolute within-group correlation.
#' @param max_vif Maximum within-group variance inflation factor.
#' @return A list containing selected names, a column-level audit, status, and
#' an explicit selection error message when `collinear_select()` fails.
#' @examples
#' select_local_hvarpart_predictor_group(
#'   data.frame(a = 1:10, b = stats::rnorm(10)),
#'   candidates = c("a", "b"), preference = c("a", "b"),
#'   group_name = "human"
#' )
select_local_hvarpart_predictor_group <- function(
  data_source,
  candidates,
  preference = candidates,
  group_name,
  max_cor = 0.8,
  max_vif = 5
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(candidates),
    length(candidates) > 0L,
    all(candidates %in% names(data_source)),
    setequal(candidates, preference),
    assertthat::is.string(group_name),
    is.numeric(max_cor), length(max_cor) == 1L,
    max_cor > 0, max_cor <= 1,
    is.numeric(max_vif), length(max_vif) == 1L, max_vif >= 1,
    msg = "Local predictor-group selection inputs do not satisfy the contract."
  )

  validity <- purrr::map_dfr(
    candidates,
    .f = ~ {
      values <- data_source[[.x]]
      finite_values <- values[is.finite(values)]
      reason <- dplyr::case_when(
        length(values) == 0L ~ "empty",
        any(!is.finite(values)) ~ "non_finite",
        length(finite_values) == 0L ~ "empty",
        dplyr::n_distinct(finite_values) <= 1L ~ "constant",
        .default = "candidate"
      )
      tibble::tibble(
        group = group_name,
        predictor = .x,
        preference_rank = match(.x, preference),
        preselection_status = reason
      )
    }
  )
  valid <- validity |>
    dplyr::filter(.data[["preselection_status"]] == "candidate") |>
    dplyr::arrange(.data[["preference_rank"]]) |>
    dplyr::pull("predictor")
  insufficient_rows <- nrow(data_source) < 3L

  selection_attempt <- if (length(valid) <= 1L || insufficient_rows) {
    list(result = valid, error = NULL)
  } else {
    purrr::safely(collinear::collinear_select)(
      df = data_source,
      response = NULL,
      predictors = valid,
      preference_order = valid,
      max_cor = max_cor,
      max_vif = max_vif,
      quiet = TRUE
    )
  }
  selection_error <- if (is.null(selection_attempt$error)) {
    NA_character_
  } else {
    conditionMessage(selection_attempt$error)
  }
  selected <- if (is.na(selection_error)) {
    as.character(selection_attempt$result)
  } else {
    character()
  }

  audit <- validity |>
    dplyr::mutate(
      selected = .data[["predictor"]] %in% selected,
      reason = dplyr::case_when(
        !is.na(.env[["selection_error"]]) &
          .data[["preselection_status"]] == "candidate" ~ "selection_error",
        .data[["selected"]] & insufficient_rows ~
          "retained_unfiltered_insufficient_rows",
        .data[["selected"]] ~ "retained",
        .data[["preselection_status"]] != "candidate" ~
          .data[["preselection_status"]],
        .default = "within_group_collinearity"
      )
    )
  status <- dplyr::case_when(
    !is.na(selection_error) ~ "selection_error",
    length(selected) == 0L ~ paste0("missing_", group_name, "_predictor"),
    .default = "eligible_for_design_check"
  )
  res <- list(
    selected = selected,
    audit = audit,
    status = status,
    error_message = selection_error
  )

  return(res)
}
