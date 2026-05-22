#' @title Prepare predictor and m2 data for H2 hvar analysis
#' @description
#' Reshape predictor data to wide nested tables, merge predictor tracks by age,
#' and align merged predictor ages with available `m2` matrix columns.
#' @param data_predictors
#' Data frame with columns `region`, `climatezone`, `variable`, `age`, and
#' `value`.
#' @param data_m2 Data frame with columns `region`, `climatezone`, and `m2`.
#' @return Data frame with columns `region`, `climatezone`, `data_merge`, `m2`.
get_data_for_h2_hvar <- function(data_predictors, data_m2) {
  assertthat::assert_that(
    is.data.frame(data_predictors),
    msg = "`data_predictors` must be a data frame."
  )

  assertthat::assert_that(
    is.data.frame(data_m2),
    msg = "`data_m2` must be a data frame."
  )

  assertthat::assert_that(
    all(c("region", "climatezone", "variable", "age", "value") %in% names(data_predictors)),
    msg = "`data_predictors` must contain `region`, `climatezone`, `variable`, `age`, and `value`."
  )

  assertthat::assert_that(
    all(c("region", "climatezone", "m2") %in% names(data_m2)),
    msg = "`data_m2` must contain `region`, `climatezone`, and `m2`."
  )

  data_pred_work <-
    data_predictors %>%
    dplyr::rename(
      predictor = variable,
      fit = value
    ) %>%
    dplyr::group_by(predictor, region, climatezone) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = predictor,
      values_from = data
    )

  predictor_names <-
    c("temp_annual", "temp_cold", "prec_summer", "prec_win", "spd")

  missing_predictor_names <-
    setdiff(predictor_names, names(data_pred_work))

  if (length(missing_predictor_names) > 0) {
    for (predictor_name in missing_predictor_names) {
      data_pred_work[[predictor_name]] <-
        rep(list(NULL), nrow(data_pred_work))
    }
  }

  data_pred_work <-
    data_pred_work %>%
    dplyr::mutate(
      temp_annual = purrr::map(
        .x = temp_annual,
        .f = purrr::possibly(
          ~ .x %>%
            dplyr::select(age, fit) %>%
            dplyr::rename(
              temp_annual = fit
            )
        )
      ),
      temp_cold = purrr::map(
        .x = temp_cold,
        .f = purrr::possibly(
          ~ .x %>%
            dplyr::select(age, fit) %>%
            dplyr::rename(
              temp_cold = fit
            )
        )
      ),
      prec_summer = purrr::map(
        .x = prec_summer,
        .f = purrr::possibly(
          ~ .x %>%
            dplyr::select(age, fit) %>%
            dplyr::rename(
              prec_summer = fit
            )
        )
      ),
      prec_win = purrr::map(
        .x = prec_win,
        .f = purrr::possibly(
          ~ .x %>%
            dplyr::select(age, fit) %>%
            dplyr::rename(
              prec_win = fit
            )
        )
      ),
      spd = purrr::map(
        .x = spd,
        .f = purrr::possibly(
          ~ .x %>%
            dplyr::select(age, fit) %>%
            dplyr::rename(
              spd = fit
            )
        )
      ),
      data_merge = purrr::pmap(
        .l = list(
          temp_annual, # ..1
          temp_cold, # ..2
          prec_summer, # ..3
          prec_win, # ..4
          spd # ..5
        ),
        .f = purrr::possibly(
          ~ dplyr::inner_join(
            ..1,
            ..2,
            by = "age"
          ) %>%
            dplyr::inner_join(
              ..3,
              by = "age"
            ) %>%
            dplyr::inner_join(
              ..4,
              by = "age"
            ) %>%
            dplyr::inner_join(
              ..5,
              by = "age"
            ) %>%
            tidyr::drop_na() %>%
            as.data.frame(stringsAsFactors = FALSE),
          otherwise = data.frame()
        )
      )
    )

  res_data <-
    dplyr::inner_join(
    data_pred_work,
    data_m2,
    by = dplyr::join_by(
      "region", "climatezone"
    )
  ) %>%
    # subset the data so that the yshare same time period
    dplyr::mutate(
      data_merge = purrr::map2(
        .x = data_merge,
        .y = m2,
        .f = purrr::possibly(
          ~ if ("age" %in% names(.x)) {
            .x %>%
              dplyr::filter(age %in% colnames(.y))
          } else {
            data.frame()
          },
          otherwise = data.frame()
        )
      )
    ) %>%
    dplyr::select(
      region, climatezone, data_merge, m2
    )

  return(res_data)
}
