fit_hgam <- function(
    data_list,
    use_parallel = FALSE,
    verbose = FALSE) {
  sel_data <-
    data_list %>%
    purrr::chuck("sel_data")

  if (
    nrow(sel_data) > 0
  ) {
    n_recors <-
      sel_data$dataset_id %>%
      unique() %>%
      length()

    message(n_recors)

    use_parallel <-
      parallel::detectCores() < n_recors

    # Fit GAM model
    data_mod <-
      REcopol::fit_hgam(
        data_source = sel_data,
        x_var = data_list %>%
          purrr::chuck("x_var"),
        y_var = y_var %>%
          purrr::chuck("y_var"),
        group_var = group_var %>%
          purrr::chuck("group_var"),
        weights_var = weights_var %>%
          purrr::chuck("weights_var"),
        smooth_basis = smooth_basis %>%
          purrr::chuck("smooth_basis"),
        error_family = error_family %>%
          purrr::chuck("error_family"),
        sel_k = sel_k %>%
          purrr::chuck("sel_k"),
        common_trend = TRUE,
        use_parallel = use_parallel,
        max_iterations = 200,
        verbose = verbose
      )

    return(data_mod)
  }
}
