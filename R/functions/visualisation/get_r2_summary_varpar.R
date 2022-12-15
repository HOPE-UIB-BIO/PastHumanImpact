get_r2_summary_varpar <- function(data_source,
                                  sel_var,
                                  group_vars = c(
                                    "region",
                                    "ecozone_koppen_5"
                                  )) {
  assertthat::assert_that(
    sel_var %in% names(data_source),
    msg = "'sel_var' has to be a valid column in 'data_source'"
  )

  data_meta_sum <-
    data_source %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_vars)
      )
    ) %>%
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        c("long", "lat"),
        list(
          mean = ~ mean(.x, na.rm = TRUE)
        )
      )
    )

  data_r2_sum <-
    data_source %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_vars)
      )
    ) %>%
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(sel_var),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          upr = ~ stats::quantile(.x, 0.975, na.rm = TRUE),
          lwr = ~ stats::quantile(.x, 0.025, na.rm = TRUE)
        )
      )
    )

  dplyr::left_join(
    data_meta_sum,
    data_r2_sum,
    by = group_vars
  ) %>%
    return()
}
