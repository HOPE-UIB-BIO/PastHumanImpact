get_all_predicted_general_trends <- function(data_source, sel_type = c("predictors", "events")) {
  sel_type <- match.arg(sel_type)

  data_work <-
    data_source %>%
    # dplyr::filter(
    #   need_to_run == FALSE &
    #     need_to_be_evaluated == FALSE
    # ) %>%
    dplyr::mutate(
      is_h2_predictor = dplyr::case_when(
        .default = FALSE,
        variable == "temp_annual" ~ TRUE,
        variable == "temp_cold" ~ TRUE,
        variable == "prec_summer" ~ TRUE,
        variable == "prec_win" ~ TRUE,
        variable == "spd" ~ TRUE
      )
    )

  if (
    sel_type == "predictors"
  ) {
    data_selected <-
      data_work %>%
      dplyr::filter(
        is_h2_predictor == TRUE
      )
  } else if (
    sel_type == "events"
  ) {
    data_selected <-
      data_work %>%
      dplyr::filter(
        is_h2_predictor == FALSE
      )
  } else {
    stop("sel_type not recognized")
  }

  purrr::pmap(
    .progress = "loading general trends models",
    .l = list(
      data_selected$region, # ..1
      data_selected$climatezone, # ..2
      data_selected$variable # ..3
    ),
    .f = ~ {
      sel_region <- ..1
      sel_climatezone <- ..2
      sel_variable <- ..3

      sel_file_name <-
        paste(
          sel_variable,
          sel_region,
          sel_climatezone,
          sep = "__"
        )

      predictor_models_dir <-
        paste0(
          data_storage_path,
          "Data/Predictor_models/"
        )

      general_trends_dir <-
        paste0(
          predictor_models_dir,
          "General_trends"
        )

      data_predicted <-
        RUtilpol::get_latest_file(
          file_name = sel_file_name,
          dir = general_trends_dir,
          verbose = FALSE
        )

      return(data_predicted)
    }
  ) %>%
    dplyr::bind_rows() %>%
    return()
}
