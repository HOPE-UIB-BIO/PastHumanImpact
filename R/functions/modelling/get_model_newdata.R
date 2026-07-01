#' @title Build prediction data for a temporal model
#' @description
#' Build one prediction grid by `region x climatezone x stratum` and age for a
#' selected model configuration row.
#' @param data_source Long model data.
#' @param model_config_row One-row model configuration data frame.
#' @return Tibble suitable for `brms::fitted()`.
#' @examples
#' \dontrun{
#' data_new <- get_model_newdata(data_source, model_config_row)
#' }
get_model_newdata <- function(data_source, model_config_row) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(model_config_row),
    nrow(model_config_row) == 1,
    msg = "`model_config_row` must be a one-row data frame."
  )

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

  data_strata <-
    data_source %>%
    dplyr::filter(variable == sel_variable)

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

  data_strata <-
    data_strata %>%
    dplyr::group_by(region, climatezone, .data[[stratum_var]]) %>%
    dplyr::summarise(
      dataset_id = as.character(dplyr::first(.data[[group_var]])),
      .groups = "drop"
    )

  data_ages <-
    tibble::tibble(
      age = seq(
        from = model_config_row[["age_min"]][1],
        to = model_config_row[["age_max"]][1],
        by = model_config_row[["timestep"]][1]
      )
    ) %>%
    dplyr::mutate(age_ka = age / 1000)

  res_newdata <-
    tidyr::crossing(
      data_strata,
      data_ages
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(group_var, stratum_var)),
        as.factor
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
