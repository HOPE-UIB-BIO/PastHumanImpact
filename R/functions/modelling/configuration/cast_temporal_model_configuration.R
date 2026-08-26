#' @title Cast temporal lifecycle configuration
#' @description
#' Align persisted lifecycle column types with a candidate configuration.
#' @param data_current Persisted lifecycle configuration.
#' @param data_candidate Candidate lifecycle configuration.
#' @return Type-aligned persisted configuration tibble.
#' @examples
#' \dontrun{
#' aligned <- cast_temporal_model_configuration(current, candidate)
#' }
cast_temporal_model_configuration <- function(
  data_current,
  data_candidate
) {
  assertthat::assert_that(
    is.data.frame(data_current),
    is.data.frame(data_candidate),
    all(names(data_candidate) %in% names(data_current)),
    msg = "Temporal configurations do not share required columns."
  )

  res_config <-
    purrr::map2(
      .x = data_current[names(data_candidate)],
      .y = data_candidate,
      .f = cast_temporal_config_column
    ) |>
    dplyr::bind_cols()

  return(res_config)
}
