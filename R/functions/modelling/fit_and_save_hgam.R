fit_and_save_hgam <- function(data_raw,
                              sel_group,
                              dir = here::here(),
                              x_var = "age",
                              y_var,
                              group_var = "dataset_id",
                              weights_var = NULL,
                              smooth_basis = "tp",
                              error_family,
                              sel_k,
                              use_parallel = FALSE,
                              verbose = FALSE,
                              save = TRUE) {
  message(y_var)

  sel_data <-
    data_raw %>%
    dplyr::filter(group == sel_group | region == sel_group) %>%
    dplyr::mutate(
      dataset_id = as.factor(dataset_id)
    )

  message(sel_group)

  sel_data$dataset_id %>%
    unique() %>%
    length() %>%
    message()

  if (
    nrow(sel_data) > 0
  ) {
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

    if (
      isTRUE(save)
    ) {
      RUtilpol::save_latest_file(
        object_to_save = data_mod,
        file_name = paste0(
          y_var,
          "_",
          sel_group
        ),
        dir = dir,
        prefered_format = "rds",
        use_sha = TRUE
      )
    } else {
      return(data_mod)
    }
  }
}
