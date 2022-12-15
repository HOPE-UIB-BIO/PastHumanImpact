get_r2_summary_varpar <- function(data_source,
                                    group_vars = c(
                                      "region",
                                      "ecozone_koppen_5"
                                    )) {
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
          c("unique", "individual", "average_share", "i_perc_percent"),
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