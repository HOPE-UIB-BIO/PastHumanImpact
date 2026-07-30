#' @title Build prediction data for a temporal model
#' @description
#' Build one prediction grid by region, climate zone, stratum, dataset, and age
#' for a selected model configuration row.
#' @param data_source Long model data.
#' @param model_config_row One-row model configuration data frame.
#' @param prediction_range Character scalar. Use `"configured"` for every
#' dataset over the configured age range, or `"group_observed"` to restrict
#' each dataset to its observed age range.
#' @return Tibble suitable for `brms::posterior_epred()`.
#' @examples
#' \dontrun{
#' data_new <- get_model_newdata(data_source, model_config_row)
#' }
get_model_newdata <- function(
  data_source,
  model_config_row,
  prediction_range = c("configured", "group_observed")
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(model_config_row),
    nrow(model_config_row) == 1,
    msg = "`model_config_row` must be a one-row data frame."
  )

  prediction_range <-
    match.arg(prediction_range)

  required_config_cols <-
    c(
      "variable",
      "x_var",
      "x_model_var",
      "x_mean",
      "x_sd",
      "group_var",
      "stratum_var",
      "age_min",
      "age_max",
      "timestep"
    )

  assertthat::assert_that(
    all(required_config_cols %in% names(model_config_row)),
    msg = "`model_config_row` is missing required prediction config columns."
  )

  sel_variable <- model_config_row[["variable"]][1]
  group_var <- model_config_row[["group_var"]][1]
  stratum_var <- model_config_row[["stratum_var"]][1]
  x_var <- model_config_row[["x_var"]][1]
  x_model_var <- model_config_row[["x_model_var"]][1]

  required_data_cols <-
    c(
      "variable",
      "age",
      "region",
      "climatezone",
      stratum_var,
      group_var,
      x_var
    )

  assertthat::assert_that(
    all(required_data_cols %in% names(data_source)),
    msg = "`data_source` is missing required prediction columns."
  )

  data_strata <-
    data_source %>%
    dplyr::filter(variable == sel_variable)

  if (identical(sel_variable, "spd")) {
    data_strata <-
      data_strata %>%
      dplyr::filter(.data[["age"]] >= 2000)
  }

  if (
    "analysis" %in% names(model_config_row) &&
      "analysis" %in% names(data_strata)
  ) {
    data_strata <-
      data_strata %>%
      dplyr::filter(
        analysis == model_config_row[["analysis"]][1]
      )
  }

  if (
    all(c("region", "climatezone") %in% names(model_config_row))
  ) {
    data_strata <-
      data_strata %>%
      dplyr::filter(
        region == model_config_row[["region"]][1],
        climatezone == model_config_row[["climatezone"]][1]
      )
  }

  grouping_columns <-
    c("region", "climatezone", stratum_var, group_var)

  if (
    prediction_range == "configured"
  ) {
    data_strata <-
      data_strata %>%
      dplyr::select(dplyr::all_of(grouping_columns)) %>%
      dplyr::distinct()
    data_ages <-
      tibble::tibble(
        age = seq(
          from = if (identical(sel_variable, "spd")) {
            max(model_config_row[["age_min"]][1], 2000)
          } else {
            model_config_row[["age_min"]][1]
          },
          to = model_config_row[["age_max"]][1],
          by = model_config_row[["timestep"]][1]
        )
      )

    res_newdata <-
      tidyr::crossing(data_strata, data_ages)
  } else {
    res_newdata <-
      data_strata %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(grouping_columns))
      ) %>%
      dplyr::summarise(
        age_min = max(
          min(age),
          model_config_row[["age_min"]][1]
        ),
        age_max = min(
          max(age),
          model_config_row[["age_max"]][1]
        ),
        .groups = "drop"
      ) %>%
      dplyr::filter(age_min <= age_max) %>%
      dplyr::mutate(
        age = purrr::map2(
          age_min,
          age_max,
          ~ seq(
            from = .x,
            to = .y,
            by = model_config_row[["timestep"]][1]
          )
        )
      ) %>%
      tidyr::unnest(age) %>%
      dplyr::select(-age_min, -age_max)
  }

  res_newdata <-
    res_newdata %>%
    dplyr::mutate(
      age_ka = age / 1000,
      dplyr::across(
        dplyr::all_of(c(group_var, stratum_var)),
        ~ factor(as.character(.x))
      )
    )

  assertthat::assert_that(
    x_var %in% names(res_newdata),
    msg = "`x_var` must be created in prediction newdata."
  )
  assertthat::assert_that(
    nrow(res_newdata) > 0,
    msg = "`data_source` has no rows for the selected prediction config."
  )

  res_newdata <-
    standardise_model_predictor(
      data_source = res_newdata,
      x_var = x_var,
      x_model_var = x_model_var,
      x_mean = model_config_row[["x_mean"]][1],
      x_sd = model_config_row[["x_sd"]][1]
    )

  x_back_transformed <-
    res_newdata[[x_model_var]] * model_config_row[["x_sd"]][1] +
    model_config_row[["x_mean"]][1]

  assertthat::assert_that(
    isTRUE(
      all.equal(
        x_back_transformed,
        res_newdata[[x_var]],
        tolerance = sqrt(.Machine$double.eps)
      )
    ),
    msg = "The standardised predictor cannot be back-transformed exactly."
  )

  return(res_newdata)
}
