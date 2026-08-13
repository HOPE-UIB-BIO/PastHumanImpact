#' @title Scale age within one dataset
#' @description Center and scale a finite age variable for temporal control.
#' @param data_source One dataset-level data frame.
#' @param age_col Age column.
#' @param output_col Name for the standardised age column.
#' @return The input data with one standardised age column.
#' @examples
#' \dontrun{
#' scale_temporal_age(tibble::tibble(age = seq(0, 1000, 500)))
#' }
scale_temporal_age <- function(
  data_source,
  age_col = "age",
  output_col = "time"
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    age_col %in% names(data_source),
    is.numeric(data_source[[age_col]]),
    all(is.finite(data_source[[age_col]])),
    dplyr::n_distinct(data_source[[age_col]]) > 1L,
    assertthat::is.string(output_col),
    msg = "Temporal age inputs do not satisfy the required contract."
  )

  age_sd <- stats::sd(data_source[[age_col]])

  if (
    !is.finite(age_sd) || age_sd <= 0
  ) {
    cli::cli_abort("Age must have finite, positive variation.")
  }

  res_data <-
    data_source |>
    dplyr::mutate(
      !!output_col :=
        (.data[[age_col]] - mean(.data[[age_col]])) / age_sd
    )

  return(res_data)
}
