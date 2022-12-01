subset_event_types <- function(data_source_events,
                               data_source_meta,
                               data_source_dummy_time) {
  events_prepared <-
    data_source_meta %>%
    dplyr::select(dataset_id, region) %>%
    dplyr::left_join(
      data_source_events,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      have_events = purrr::map_lgl(
        .x = events_updated,
        .f = tibble::is_tibble
      )
    ) %>%
    dplyr::mutate(
      events = purrr::pmap(
        .l = list(
          have_events, # ..1
          region, # ..2
          events_updated, # ..3
          dataset_id
        ),
        .f = ~ ifelse(
          test = isTRUE(..1),
          # if there is a valid table with events
          yes = switch(
            # based on the region
            ..2,
            "Asia" = return(
              dplyr::select(
                .data = ..3,
                dplyr::any_of(
                  c("age", "bi", "fi", "fc", "ei")
                )
              )
            ),
            "Europe" = return(
              dplyr::select(
                .data = ..3,
                dplyr::any_of(
                  c("age", "bi", "fi", "fc", "ec")
                )
              )
            ),
            "North America" = return(
              dplyr::select(
                .data = ..3,
                dplyr::any_of(
                  c("age", "bi", "fc", "es")
                )
              )
            ),
            "Latin America" = return(
              dplyr::select(
                .data = ..3,
                dplyr::any_of(
                  c("age", "no_impact", "weak", "strong")
                )
              )
            ),
            "Oceania" = return(
              dplyr::select(
                .data = ..3,
                dplyr::any_of(
                  c("age", "no_impact", "weak", "medium", "strong")
                )
              )
            ),
            return(NA)
          ),
          # if there is NOT valid table with events
          no = switch(
            # based on the region
            ..2,
            "Asia" = return(
              data_source_dummy_time %>%
                dplyr::mutate(
                  bi = 1,
                  fi = 0,
                  fc = 0,
                  ei = 0
                )
            ),
            "Europe" = return(
              data_source_dummy_time %>%
                dplyr::mutate(
                  bi = 1,
                  fi = 0,
                  fc = 0,
                  ec = 0
                )
            ),
            "North America" = return(
              data_source_dummy_time %>%
                dplyr::mutate(
                  bi = 1,
                  fc = 0,
                  es = 0
                )
            ),
            "Latin America" = return(
              data_source_dummy_time %>%
                dplyr::mutate(
                  no_impact = 1,
                  weak = 0,
                  strong = 0
                )
            ),
            "Oceania" = return(
              data_source_dummy_time %>%
                dplyr::mutate(
                  no_impact = 1,
                  weak = 0,
                  medium = 0,
                  strong = 0
                )
            ),
            return(NA)
          )
        )
      )
    ) %>%
    dplyr::select(dataset_id, have_events, events)

  return(events_prepared)
}
