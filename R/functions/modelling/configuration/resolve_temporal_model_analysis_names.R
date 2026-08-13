#' @title Resolve Temporal Model Analysis Names
#' @description
#' Translate a user-facing temporal model group into stored analysis names.
#' @param sel_type Character scalar. One of `predictors`, `events`, `paps`, or
#' `all`.
#' @return Character vector of analysis names.
resolve_temporal_model_analysis_names <- function(
  sel_type = c("predictors", "events", "paps", "all")
) {
  assertthat::assert_that(
    is.character(sel_type),
    length(sel_type) >= 1L,
    !anyNA(sel_type),
    msg = "`sel_type` must contain a temporal analysis-group name."
  )

  sel_type <- match.arg(sel_type)

  res_names <-
    switch(
      sel_type,
      predictors = "predictor_temporal",
      events = "event_temporal",
      paps = "pap_temporal",
      all = c("predictor_temporal", "event_temporal", "pap_temporal")
    )

  return(res_names)
}
