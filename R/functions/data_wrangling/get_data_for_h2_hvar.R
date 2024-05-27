get_data_for_h2_hvar <- function(data_predictors, data_m2) {
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
    ) %>%
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
            drop_na(),
          otherwise = data.frame()
        )
      )
    )

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
          ~ .x %>%
            dplyr::filter(age %in% colnames(.y))
        )
      )
    ) %>%
    dplyr::select(
      region, climatezone, data_merge, m2
    ) %>%
    return()
}
