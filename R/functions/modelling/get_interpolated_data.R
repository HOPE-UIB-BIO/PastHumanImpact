#' @title Interpolate nested modelling time series
#' @description
#' Interpolates nested input series by grouping variable and reshapes output to
#' wide nested tables by variable name.
#' @param data_source Data frame containing nested `data_to_fit` tables.
#' @param variable Name of the variable column in nested data.
#' @param vars_interpolate Character vector of x and y columns for interpolation.
#' @param group_var Grouping column name used to split series.
#' @param method Interpolation method passed to `stats::approx()`.
#' @param rule Extrapolation rule passed to `stats::approx()`.
#' @param ties Tie handling passed to `stats::approx()`.
#' @param age_min Minimum output age.
#' @param age_max Maximum output age.
#' @param timestep Output timestep.
#' @param verbose Logical; if `TRUE`, prints progress comments.
#' @return Data frame with `dataset_id` and nested interpolated `data` tables.
get_interpolated_data <- function(data_source,
                                  variable = "var_name",
                                  vars_interpolate = c("age", "value"),
                                  group_var = "dataset_id",
                                  method = c("constant", "linear"),
                                  rule = 1:2,
                                  ties = "ordered",
                                  age_min = 0,
                                  age_max = 12e03,
                                  timestep = 500,
                                  verbose = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    "data_to_fit" %in% names(data_source),
    msg = "`data_source` must contain `data_to_fit`."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_source$data_to_fit, is.data.frame)),
    msg = "`data_to_fit` entries must be data frames."
  )
  assertthat::assert_that(
    is.character(variable) && length(variable) == 1,
    msg = "`variable` must be a single character value."
  )
  assertthat::assert_that(
    is.character(vars_interpolate),
    msg = "`vars_interpolate` must be character."
  )
  assertthat::assert_that(
    is.character(group_var) && length(group_var) == 1,
    msg = "`group_var` must be a single character value."
  )
  assertthat::assert_that(
    is.character(method) && length(method) == 1,
    msg = "`method` must be a single character value."
  )
  assertthat::assert_that(
    is.logical(verbose) && length(verbose) == 1,
    msg = "`verbose` must be a single logical value."
  )

  RUtilpol::check_class("variable", "character")
  RUtilpol::check_class("vars_interpolate", "character")
  RUtilpol::check_class("method", "character")
  RUtilpol::check_class("data_source", "data.frame")
  RUtilpol::check_class("verbose", "logical")

  if (isTRUE(verbose)) {
    RUtilpol::output_comment("getting linear interpolated values")
  }

  n_datasets <-
    data_source %>%
    tidyr::unnest(data_to_fit) %>%
    dplyr::distinct(get(group_var)) %>%
    purrr::pluck(1) %>%
    length()

  if (isTRUE(verbose)) {
    RUtilpol::output_comment(paste("N datasets:", n_datasets))
  }

  suppressWarnings(
    res <-
      data_source %>%
      tidyr::unnest(data_to_fit) %>%
      dplyr::select(dplyr::all_of(c(group_var, variable, vars_interpolate))) %>%
      dplyr::group_by(get(group_var)) %>%
      tidyr::nest(data = dplyr::any_of(vars_interpolate)) %>%
      tidyr::drop_na(data) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        n_levels = purrr::map_dbl(
          .x = data,
          .f = nrow
        )
      ) %>%
      dplyr::mutate(
        mod = purrr::pmap(
          .l = list(
            data,
            get(group_var),
            n_levels
          ),
          .f = ~ {
            if (isTRUE(verbose)) {
              message(..2)
            }
            approx(xy.coords(..1),
              xout = seq(
                age_min,
                age_max,
                by = timestep
              ),
              ties = ties,
              method = method,
              rule = rule
            )
          }
        )
      )
  )

  # new interpolated data
  data_interpolated <- res %>%
    dplyr::mutate(
      int_data = purrr::map(
        .x = mod,
        .f = ~ {
          data.frame(.x) %>%
            rename(
              age = x,
              fit = y
            ) %>%
            as_tibble() %>%
            return()
        }
      )
    ) %>%
    tidyr::unnest(int_data) %>%
    dplyr::select(
      var_name, dataset_id, age, fit
    ) %>%
    tidyr::pivot_wider(
      names_from = var_name,
      values_from = "fit"
    ) %>%
    tidyr::nest(data = -c(dataset_id))

  return(data_interpolated)
}
