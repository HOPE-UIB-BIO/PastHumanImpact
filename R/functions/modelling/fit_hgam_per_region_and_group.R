fit_hgam_per_region_and_group <- function(
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
    sel_k,
    use_parallel = FALSE,
    verbose = FALSE) {
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

  if (
    nrow(sel_data) > 0
  ) {
    sel_data$dataset_id %>%
      unique() %>%
      length() %>%
      message()

    # Fit GAM model
    data_mod <-
      REcopol::fit_hgam(
        data_source = sel_data,
        x_var = x_var,
        y_var = y_var,
        group_var = group_var,
        weights_var = weights_var,
        smooth_basis = smooth_basis,
        error_family = error_family,
        sel_k = sel_k,
        common_trend = TRUE,
        use_parallel = use_parallel,
        max_iterations = 200,
        verbose = verbose
      )

    return(data_mod)
  }
}
