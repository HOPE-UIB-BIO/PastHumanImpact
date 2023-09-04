get_data_for_indiv_hgams <- function(
    data_raw,
    data_error,
    sel_group,
    sel_region,
    x_var = "age",
    y_var,
    group_var = "dataset_id",
    weights_var = NULL,
    smooth_basis = "tp",
    error_family,
    sel_k) {
  sel_data <-
    data_raw %>%
    dplyr::filter(group == sel_group) %>%
    dplyr::filter(region == sel_region) %>%
    dplyr::mutate(
      dataset_id = as.factor(dataset_id)
    )

  error_family <-
    data_error %>%
    dplyr::filter(predictor == y_var) %>%
    purrr::chuck("error_family", 1)

  list(
    sel_data = sel_data,
    error_family = error_family,
    x_var = x_var,
    y_var = y_var,
    group_var = group_var,
    weights_var = weights_var,
    smooth_basis = smooth_basis,
    sel_k = sel_k
  ) %>%
    return()
}
