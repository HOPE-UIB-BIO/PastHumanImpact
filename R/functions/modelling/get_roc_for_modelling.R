#' @title Prepare ROC data for modelling
#' @description
#' Unnest ROC tables, compute per-record weights from ROC error, pivot to long
#' format, and nest modelling data by variable.
#' @param data_source_roc Data frame with list-column `PAP_roc`.
#' @return Data frame with `var_name` and nested `data_to_fit` tables.
get_roc_for_modelling <- function(data_source_roc) {
  assertthat::assert_that(
    is.data.frame(data_source_roc),
    msg = "`data_source_roc` must be a data frame."
  )

  assertthat::assert_that(
    "PAP_roc" %in% names(data_source_roc),
    msg = "`data_source_roc` must contain `PAP_roc`."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(data_source_roc$PAP_roc, is.data.frame)),
    msg = "`PAP_roc` entries must be data frames."
  )

  res_data <-
    data_source_roc %>%
    tidyr::unnest(PAP_roc) %>%
    dplyr::mutate(
      roc_error = abs(ROC_up - ROC_dw)
    ) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(
      roc_error_avg = mean(roc_error),
      var_weight = roc_error_avg / roc_error
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dataset_id,
      age = Age,
      roc = ROC,
      var_weight
    ) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age, var_weight),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value, var_weight))

  return(res_data)
}
