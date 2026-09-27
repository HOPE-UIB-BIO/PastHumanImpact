#' @title Select local HVarPart predictors within conceptual groups
#' @description Remove invalid columns and apply response-independent
#' `collinear` correlation/VIF filtering separately to human and climate
#' candidates with deterministic preference ordering.
#' @param data_source Data frame for one analytical unit.
#' @param human_candidates Candidate human predictor names.
#' @param climate_candidates Candidate climate predictor names.
#' @param human_preference Human predictor preference order.
#' @param climate_preference Climate predictor preference order.
#' @param max_cor Maximum absolute within-group correlation.
#' @param max_vif Maximum within-group variance inflation factor.
#' @return A list with selected predictor groups and a column-level audit.
#' @examples
#' select_local_hvarpart_predictors(
#'   data.frame(h = 1:10, c = stats::rnorm(10)), "h", "c", "h", "c"
#' )
select_local_hvarpart_predictors <- function(
  data_source,
  human_candidates,
  climate_candidates,
  human_preference = human_candidates,
  climate_preference = climate_candidates,
  max_cor = 0.8,
  max_vif = 5
) {
  candidates <- unique(c(human_candidates, climate_candidates))
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(human_candidates),
    is.character(climate_candidates),
    all(candidates %in% names(data_source)),
    setequal(human_candidates, human_preference),
    setequal(climate_candidates, climate_preference),
    is.numeric(max_cor), max_cor > 0, max_cor <= 1,
    is.numeric(max_vif), max_vif >= 1,
    msg = "Local HVarPart predictor selection inputs do not satisfy the contract."
  )

  human <- select_local_hvarpart_predictor_group(
    data_source = data_source,
    candidates = human_candidates,
    preference = human_preference,
    group_name = "human",
    max_cor = max_cor,
    max_vif = max_vif
  )
  climate <- select_local_hvarpart_predictor_group(
    data_source = data_source,
    candidates = climate_candidates,
    preference = climate_preference,
    group_name = "climate",
    max_cor = max_cor,
    max_vif = max_vif
  )
  predictor_vars <- list(human = human$selected, climate = climate$selected)
  status <- dplyr::case_when(
    human$status == "selection_error" ~ "human_selection_error",
    climate$status == "selection_error" ~ "climate_selection_error",
    length(predictor_vars$human) == 0L ~ "missing_human_predictor",
    length(predictor_vars$climate) == 0L ~ "missing_climate_predictor",
    .default = "eligible_for_design_check"
  )
  res <- list(
    status = status,
    predictor_vars = predictor_vars,
    audit = dplyr::bind_rows(human$audit, climate$audit),
    thresholds = c(max_cor = max_cor, max_vif = max_vif),
    selection_errors = tibble::tibble(
      group = c("human", "climate"),
      error_message = c(human$error_message, climate$error_message)
    ) |>
      dplyr::filter(!is.na(.data[["error_message"]]))
  )

  return(res)
}
