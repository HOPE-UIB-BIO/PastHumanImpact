#' @title Get Temporal Model Analysis Names
#' @description
#' Translate a user-facing temporal model group into stored analysis names.
#' @param sel_type Character scalar. One of `predictors`, `events`, `paps`, or
#' `all`.
#' @return Character vector of analysis names.
get_temporal_model_analysis_names <- function(
  sel_type = c("predictors", "events", "paps", "all")
) {
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
