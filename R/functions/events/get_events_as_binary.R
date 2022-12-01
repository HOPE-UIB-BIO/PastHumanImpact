#' @title get binary data from event detection
#' @param data_source_events Data.file with ages of human detection
#' @param data_source_pollen Data file with filtered pollen data
#' @description For each site, get a list of ages of all levels,
#' add ages of all ages in between levels, an then simply check the start of
#' huan detection. All ages younger than that have `1`, everything else have `0`
get_events_as_binary <- function(data_source_events, data_source_pollen) {
  data_work <-
    data_source_pollen %>%
    dplyr::select(dataset_id, levels) %>%
    dplyr::inner_join(
      data_source_events,
      by = "dataset_id"
    )

  data_work_binary <-
    data_work %>%
    dplyr::mutate(
      events_binary = purrr::pmap(
        .l = list(events_age, levels, dataset_id),
        .f = ~ {
          message(..3)

          events_age <-
            ..1$age %>%
            rlang::set_names(nm = ..1$name)

          data_to_test <-
            REcopol:::util_get_inbetween_values(
              data_source = ..2$age,
              sel_output = "only_inbetween"
            ) %>%
            tibble::enframe(value = "age") %>%
            dplyr::select(age)

          res_w <-
            purrr::map2_dfc(
              .x = events_age,
              .y = names(events_age),
              .f = ~ {
                data_temp <-
                  data_to_test %>%
                  dplyr::mutate(
                    VAR = dplyr::case_when(
                      age <= .x ~ 1,
                      age > .x ~ 0,
                      is.na(.x) == TRUE ~ 0
                    )
                  )

                data_temp %>%
                  dplyr::select(-age) %>%
                  rlang::set_names(nm = .y) %>%
                  return()
              }
            )

          data_to_test %>%
            dplyr::bind_cols(res_w) %>%
            return()
        }
      )
    )

  data_work_binary %>%
    dplyr::select(dataset_id, region, events_binary) %>%
    return()
}
