#' @title Filter pollen sequences of from start of relevant time period
#' @description Filter samples based on latest age of interest
#' @return the same input data tibble but with filtered pollen data

filter_age_levels <- function(data, late_age_limit = 8.5e3) {
  data %>%
    dplyr::mutate(
      sample_ids_keep =
        purrr::map2(
          .x = levels,
          .y = end_of_interest_period,
          .f = ~ .x %>%
            dplyr::filter(age < .y) %>%
            purrr::pluck("sample_id")
        )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(levels, counts_harmonised, pollen_percentages),
        .fns = ~
          purrr::map2(.x,
            .y = sample_ids_keep,
            .f = function(.x, .y) {
              .x %>%
                dplyr::filter(sample_id %in% .y)
            }
          )
      )
    ) %>%
    dplyr::mutate(
      age_uncertainty = purrr::map2(
        .x = age_uncertainty,
        .y = sample_ids_keep,
        .f = ~ {
          data_work <-
            .x[,
              colnames(.x) %in% sample_ids_keep,
              drop = FALSE
            ]

          data_res <-
            data_work[,
              order(
                match(
                  colnames(data_work),
                  sample_ids_keep
                )
              ),
              drop = FALSE
            ]

          return(data_res)
        }
      )
    ) %>%
    dplyr::select(-sample_ids_keep)
}
