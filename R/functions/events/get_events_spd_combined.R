#' @title  select spd distance data
get_events_spd_combined <- function(data_source_events,
                                    data_source_spd,
                                    data_source_meta,
                                    data_source_dummy_time) {
  # set names to spd
  data_spd <-
    data_source_spd %>%
    mutate(data = purrr::map(
      .x = data,
      .f = ~ {
        .x %>%
          rlang::set_names(
            nm = c("age", "spd")
          ) %>%
          return()
      }
    ))

  # add missing spd data
  data_full_spd <-
    data_source_meta %>%
    dplyr::distinct(dataset_id, age_min, age_max) %>%
    dplyr::left_join(
      data_spd,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      spd = purrr::pmap(
        .l = list(
          data, # ..1
          age_min, # ..2
          age_max # ..3
        ),
        .f = ~ ifelse(
          test = isTRUE(is.null(..1)),
          yes = return(
            data_source_dummy_time %>%
              dplyr::filter(
                age >= ..2,
                age <= ..3
              ) %>%
              dplyr::mutate(
                spd = 0
              )
          ),
          no = return(..1)
        )
      )
    ) %>%
    dplyr::select(-c(age_min, age_max, data))

  # add missing events data
  dummy_data_events <-
    tibble(
      age = seq(0,
        12000,
        by = 500
      )
    ) %>%
    mutate(
      bi = 0,
      fc = 0,
      es = 0,
      fi = 0,
      ec = 0,
      cc = 0,
      no_impact = 0,
      weak = 0,
      medium = 0,
      strong = 0,
      ei = 0
    )

  data_full_events <-
    data_source_meta %>%
    dplyr::distinct(dataset_id, age_min, age_max) %>%
    dplyr::left_join(
      data_source_events,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      events = purrr::pmap(
        .l = list(
          events, # ..1
          age_min, # ..2
          age_max # ..3
        ),
        .f = ~ ifelse(
          test = isTRUE(is.na(..1)),
          yes = return(
            dummy_data_events %>%
              dplyr::filter(
                age >= ..2,
                age <= ..3
              )
          ),
          no = return(..1)
        )
      )
    ) %>%
    dplyr::select(-c(age_min, age_max, have_events))

  # merge events and spd  -----
  data_merge <-
    dplyr::inner_join(
      data_full_spd,
      data_full_events,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      data_merge = purrr::map2(
        .x = spd,
        .y = events,
        .f = ~ {
          dplyr::inner_join(
            .x %>%
              janitor::clean_names(),
            .y,
            by = "age"
          ) %>%
            dplyr::arrange(age)
        }
      )
    )

  return(data_merge)
}
