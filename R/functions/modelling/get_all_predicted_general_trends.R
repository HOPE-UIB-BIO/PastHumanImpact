#' @title Load all predicted general trend tables
#' @description
#' Load and combine stored prediction outputs for selected temporal model
#' groups.
#' @param data_source Model configuration data frame.
#' @param sel_type One of `predictors`, `events`, `paps`, or `all`.
#' @return Data frame with combined loaded prediction rows.
get_all_predicted_general_trends <- function(
  data_source,
  sel_type = c("predictors", "events", "paps", "all")
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    "variable" %in% names(data_source),
    msg = "`data_source` must contain `variable`."
  )

  assertthat::assert_that(
    exists("data_storage_path"),
    msg = "`data_storage_path` must be available in the session."
  )

  sel_type <- match.arg(sel_type)

  general_trends_dir <-
    file.path(
      data_storage_path,
      "Temporal_models",
      "General_trends"
    )

  combined_predictions_file <-
    RUtilpol::get_latest_file_name(
      file_name = "general_temporal_model_predictions",
      dir = general_trends_dir
    )

  if (
    isFALSE(is.na(combined_predictions_file))
  ) {
    data_combined_predictions <-
      RUtilpol::get_latest_file(
        file_name = "general_temporal_model_predictions",
        dir = general_trends_dir,
        verbose = FALSE
      )

    if (
      "analysis" %in% names(data_combined_predictions)
    ) {
      res_data <-
        data_combined_predictions %>%
        dplyr::filter(
          analysis %in% get_temporal_model_analysis_names(sel_type)
        )

      if (
        nrow(res_data) > 0
      ) {
        if (
          "estimate" %in% names(res_data)
        ) {
          res_data <-
            res_data %>%
            dplyr::mutate(value = estimate)
        }

        return(res_data)
      }
    }
  }

  data_work <-
    data_source

  if (
    !"analysis" %in% names(data_work)
  ) {
    data_work <-
      data_work %>%
      dplyr::mutate(
        analysis = dplyr::case_when(
          variable %in% c(
            "temp_annual",
            "temp_cold",
            "prec_summer",
            "prec_win",
            "spd"
          ) ~ "predictor_temporal",
          TRUE ~ "event_temporal"
        )
      )
  }

  if (
    !"model_id" %in% names(data_work)
  ) {
    data_work <-
      data_work %>%
      dplyr::mutate(
        model_id = stringr::str_c(analysis, variable, sep = "__")
      )
  }

  data_selected <-
    data_work %>%
    dplyr::filter(
      analysis %in% get_temporal_model_analysis_names(sel_type)
    ) %>%
    dplyr::distinct(analysis, model_id, variable)

  res_data <-
    purrr::pmap(
      .progress = "loading general trends models",
      .l = list(
        data_selected$analysis,
        data_selected$model_id,
        data_selected$variable
      ),
      .f = ~ {
        data_predicted <-
          RUtilpol::get_latest_file(
            file_name = ..2,
            dir = general_trends_dir,
            verbose = FALSE
          )

        if (
          !is.data.frame(data_predicted)
        ) {
          return(NULL)
        }

        if (
          !"analysis" %in% names(data_predicted)
        ) {
          data_predicted <-
            data_predicted %>%
            dplyr::mutate(analysis = ..1)
        }

        if (
          !"model_id" %in% names(data_predicted)
        ) {
          data_predicted <-
            data_predicted %>%
            dplyr::mutate(model_id = ..2)
        }

        if (
          !"variable" %in% names(data_predicted)
        ) {
          data_predicted <-
            data_predicted %>%
            dplyr::mutate(variable = ..3)
        }

        return(data_predicted)
      }
    ) %>%
    dplyr::bind_rows()

  return(res_data)
}
