#' @title Diagnose collinearity in one local HVarPart design
#' @description Compute pairwise correlations, VIFs, condition indices, rank,
#' and residual degrees of freedom before and after local selection. Cross-block
#' and protected-control diagnostics are reported but never used for removal.
#' @param data_source One analytical-unit data frame.
#' @param predictor_vars Selected human and climate predictor groups.
#' @param candidate_vars All focal candidates used for pre-selection diagnostics.
#' @param control_vars Optional protected time or dbMEM variables.
#' @param correlation_threshold Warning threshold for absolute correlation.
#' @param vif_threshold Warning threshold for VIF.
#' @param condition_threshold Warning threshold for condition index.
#' @return A list of pairwise, VIF, condition-index, and design diagnostics.
#' @examples
#' diagnose_local_hvarpart_collinearity(
#'   data.frame(h = 1:10, c = stats::rnorm(10)),
#'   list(human = "h", climate = "c"), c("h", "c")
#' )
diagnose_local_hvarpart_collinearity <- function(
  data_source,
  predictor_vars,
  candidate_vars,
  control_vars = character(),
  correlation_threshold = 0.8,
  vif_threshold = 5,
  condition_threshold = 30
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.list(predictor_vars),
    all(c("human", "climate") %in% names(predictor_vars)),
    is.character(candidate_vars),
    is.character(control_vars),
    is.numeric(correlation_threshold),
    is.numeric(vif_threshold),
    is.numeric(condition_threshold),
    msg = "Local HVarPart collinearity inputs do not satisfy the contract."
  )
  selected <- unique(c(unlist(predictor_vars, use.names = FALSE), control_vars))
  available_candidates <- intersect(candidate_vars, names(data_source))
  available_selected <- intersect(selected, names(data_source))
  before <- diagnose_hvarpart_design_matrix(
    data_source = data_source,
    variables = available_candidates,
    stage = "before_selection",
    vif_threshold = vif_threshold,
    condition_threshold = condition_threshold
  )
  after <- diagnose_hvarpart_design_matrix(
    data_source = data_source,
    variables = available_selected,
    stage = "after_selection_with_controls",
    vif_threshold = vif_threshold,
    condition_threshold = condition_threshold
  )
  res <- list(
    correlations = dplyr::bind_rows(
      prepare_hvarpart_pairwise_correlations(
        data_source, available_candidates, predictor_vars, control_vars,
        "before_selection", correlation_threshold
      ),
      prepare_hvarpart_pairwise_correlations(
        data_source, available_selected, predictor_vars, control_vars,
        "after_selection_with_controls", correlation_threshold
      )
    ),
    vif = dplyr::bind_rows(before$vif, after$vif),
    condition_indices = dplyr::bind_rows(before$condition, after$condition),
    design = dplyr::bind_rows(before$design, after$design)
  )

  return(res)
}
